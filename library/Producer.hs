{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Producer where
import           Control.Concurrent         hiding (yield)
import           Control.Monad.Trans.Reader

import           Data.Bits
import           Data.List


import qualified Data.ByteString            as BS


import           Network.Socket
import qualified Network.Socket.ByteString  as SBS

import           Pipes
import           Pipes.Concurrent           hiding (recv, send)
import qualified Pipes.Concurrent           as PC
import           Pipes.Lift

import qualified Pipes.Prelude              as PP

import           SecureComputation
import           Types
import           Utils

type PKey = (Key, Key)

getSocket :: IO Socket
getSocket = do
    threadDelay 10000 --for testing purposes, makes it more likely other thread will be accepting
    addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "3000")
    let serveraddr = head addrinfos
    soc <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption soc ReuseAddr 1
    connect soc (addrAddress serveraddr)
    return soc

sendList :: Socket -> [(Key, Key)] -> [Bool] -> IO()
sendList _ [] [] = return ()
sendList soc ((kp0, kp1):kps) (b:bs)= do
    let sending = if b then kp1 else kp0
    SBS.sendAll soc sending
    -- printKey (Just True) sending
    sendList soc kps bs

sendList _ _ _ = return $ error "Unbalanced send list"

producerProcessor :: (Output Key, Input Key) -> GateType -> SecurePipe
producerProcessor _ NOT = do
  (Producer a0 a1) <- await
  yield $ Producer a1 a0
producerProcessor (out, _) ty = do
  liftIO $ print ty
  (Producer a0 a1) <- await
  (Producer b0 b1) <- await
  case ty of
    XOR -> let o0 = BS.pack $ BS.zipWith xor a0 b0
               o1 = BS.pack $ BS.zipWith xor a0 b1 in
             yield (Producer o0 o1)
    _ -> do
      [AES fkey, RAND rkey] <- lift ask
      let unsorted = getTT ty (False, True) (a0, a1) (b0, b1)
      let sorted = sortBy order unsorted
      let o@(o0, o1) = mkKeyPair fkey rkey sorted
      let tt = map (insertKey o) sorted
      let encTruthTable = map (enc fkey)
      let list = encTruthTable tt
      _ <- liftIO $ PC.atomically $ PC.send out $ mconcat list
      yield $ Producer o0 o1
        where
            getTT AND (o0, o1) = helper o0 o0 o0 o1
            getTT OR (o0, o1) = helper o0 o1 o1 o1
            getTT XOR (o0, o1) = helper o0 o1 o1 o0
            getTT NOT _ = error "should not happen"
            helper o1 o2 o3 o4 (x0, x1) (y0, y1)=
              [(x0, y0, o1), (x0, y1, o2), (x1, y0, o3), (x1, y1, o4)]
            order (z1, z2, _) (z3, z4, _) =
              let p' = testBit (BS.last z1) 0
                  q' = testBit (BS.last z2) 0
                  r' = testBit (BS.last z3) 0
                  s' = testBit (BS.last z4) 0 in
                  case (compare p' r', compare q' s') of
                    (EQ, EQ) -> error "List should not contain same element"
                    (EQ, x) -> x
                    (x, _) -> x
            mkKeyPair fkey rkey ((x, y, oB):_) =
              let
                k' = hashPair fkey x y
                in mkKeyPairFromKey rkey k' oB
            mkKeyPair _ _ _ = error "Attempting to make key pair of empty list"
            insertKey (o0, o1) (x, y, bool) = if bool then (x, y, o1) else (x, y, o0)

answerProcessor :: Socket -> Pipe KeyType Bool (ReaderT [KeyContext] IO) ()
answerProcessor soc = do
  key <- await
  case key of
    (Producer a0 a1) -> do
      liftIO $ SBS.sendAll soc a0
      liftIO $ SBS.sendAll soc a1
      ans <- liftIO $ SBS.recv soc (fromIntegral cipherSize)
      let toBool
            | ans == a0 = False
            | ans == a1 = True
            | otherwise = error "Incorrect Answer Found"
      yield toBool
    (Constant b) -> yield b
    _ -> error "Producer should not receive consumer or counter"

outputProcessor :: Socket -> [(Output Key, Input Key)] -> Effect IO ()
outputProcessor soc list =
  lift $ mapM_
  (\kp -> do
      let(_, inp) = kp
      Just k <- atomically $ PC.recv inp
      SBS.sendAll soc k
      )
  list

doWithSocket :: FiniteBits a => Socket -> (a, a) -> SecureFunction -> Producer Bool IO ()
doWithSocket soc (inputProduce, inputConsume) test = do
    let (proSize, conSize) = (finiteBitSize inputProduce, finiteBitSize inputConsume)

    rkey <- liftIO genRootKey
    fkeystr <- liftIO genFixedKey
    let fkey = initFixedKey fkeystr
    liftIO $ SBS.sendAll soc fkeystr

    keyList <- liftIO $ mapM (const (genKeyPair rkey))
               [1 ..  proSize + conSize]

    let bothList = bits2Bools inputProduce ++ bits2Bools inputConsume
    liftIO $ sendList soc keyList bothList
    let secureGraph = createProgramCircuit test proSize conSize

    runReaderP [AES fkey, RAND rkey] $
      each keyList >->
      PP.map (\key -> let (a, b) = key in Producer a b) >->
      computeGraph secureGraph producerProcessor (outputProcessor soc) >->
      answerProcessor soc

doWithoutSocket :: FiniteBits a => (a, a) -> SecureFunction -> Producer Bool IO ()
doWithoutSocket input test = do
    soc <- liftIO getSocket
    doWithSocket soc input test
    liftIO $ close soc

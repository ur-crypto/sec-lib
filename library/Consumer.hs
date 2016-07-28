{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Consumer where
import           Control.Monad.Trans.Reader

import           Data.Bits
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

type CKey = Key

getSocket :: IO Socket
getSocket = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "3000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress serveraddr)
    listen sock 1
    (conn, _) <- accept sock
    return conn

consumerProcessor :: (Output Key, Input Key) -> GateType -> SecurePipe
consumerProcessor _ NOT = cat
consumerProcessor (_, inp) ty = do
  (Consumer a) <- await
  (Consumer b) <- await
  case ty of
    XOR -> yield $ Consumer (BS.pack $ BS.zipWith xor a b)
    _ -> do
      [AES fkey] <- lift ask
      Just truthTable <- liftIO $ atomically $ PC.recv inp
      let (x1, r1) = BS.splitAt (fromIntegral cipherSize) truthTable
      let (x2, r2) = BS.splitAt (fromIntegral cipherSize) r1
      let (x3, r3) = BS.splitAt (fromIntegral cipherSize) r2
      let (x4, _) = BS.splitAt (fromIntegral cipherSize) r3
      let tt = [x1, x2, x3, x4]
      let o = decTruthTable fkey a b tt
      yield $ Consumer o
        where
          decTruthTable fkey k1 k2 [o00, o01, o10, o11] =
            let k1' = testBit (BS.last k1) 0
                k2' = testBit (BS.last k2) 0 in
            let o = case (k1', k2') of
                  (False, False) -> o00
                  (False, True) -> o01
                  (True, False) -> o10
                  (True, True) -> o11
                  in
            enc fkey (k1, k2, o)
          decTruthTable _ _ _ _ = error "Incorrect number of elements passed into decTruthTable"

answerProcessor :: Socket -> Pipe KeyType Bool (ReaderT [KeyContext] IO) ()
answerProcessor soc = do
  key <- await
  case key of
    (Consumer ans) -> do
      a0 <- liftIO $ SBS.recv soc (fromIntegral cipherSize)
      a1 <- liftIO $ SBS.recv soc (fromIntegral cipherSize)
      liftIO $ SBS.sendAll soc ans
      let toBool
            | ans == a0 = False
            | ans == a1 = True
            | otherwise = error "Incorrect Answer Found"
      yield toBool
    (Constant b) -> yield b
    _ -> error "Consumer should not receive consumer or counter"

outputProcessor :: Socket -> [(Output Key, Input Key)] -> Effect IO ()
outputProcessor soc list =
  lift $ mapM_
  (\kp -> do
      let(out, _) = kp
      k <- SBS.recv soc (4 * (fromIntegral cipherSize))
      atomically $ PC.send out k
      )
  list
doWithSocket :: FiniteBits a => Socket -> (a, a) -> SecureFunction -> Producer Bool IO ()
doWithSocket soc (inputProduce, inputConsume) test = do
    let (proSize, conSize) = (finiteBitSize inputProduce, finiteBitSize inputConsume)
    let secureGraph = createProgramCircuit test proSize conSize

    fkeystr <- liftIO $ SBS.recv soc $ fromIntegral cipherSize
    let fkey = initFixedKey fkeystr
    keyList <- liftIO $ receiveList (proSize + conSize)
    runReaderP [AES fkey] $
      each keyList >->
      PP.map Consumer >->
      computeGraph secureGraph consumerProcessor (outputProcessor soc) >->
      answerProcessor soc
    where
      receiveList :: Int -> IO [Key]
      receiveList num = mapM (const $ SBS.recv soc $ fromIntegral cipherSize) [1..num]

doWithoutSocket ::FiniteBits a => (a, a) -> SecureFunction -> Producer Bool IO ()
doWithoutSocket input test = do
    soc <- liftIO getSocket
    doWithSocket soc input test
    liftIO $ close soc

-- receiveOutputs :: Literal -> GenM Bool
-- receiveOutputs (Input x') = do
--   (Consumer x) <- x'
--   let k = LBS.fromStrict x
--   o0 <- await
--   o1 <- await
--   yield k
--   return $ if
--       | k == o0 -> False
--       | k == o1 -> True
--       | otherwise -> error $ "Incorrect answer found: " ++ keyString x ++ keyString (LBS.toStrict o0) ++ keyString (LBS.toStrict o1)
-- receiveOutputs (Constant k) = return k

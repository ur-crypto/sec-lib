{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Producer where
import           Control.Concurrent             hiding (yield)
import           Control.Monad.Trans.Reader
import           Data.Bits
import qualified Data.ByteString.Lazy           as LBS
import           Network.Socket
import qualified Network.Socket.ByteString      as SBS
import qualified Network.Socket.ByteString.Lazy as LSBS

import           Pipes
import           Pipes.Lift
import qualified Pipes.Network.TCP              as P

import           System.Entropy
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
    let sending = if b
        then kp1
        else kp0
    SBS.sendAll soc sending
    -- printKey (Just True) sending
    sendList soc kps bs

sendList _ _ _ = return $ error "Unbalanced send list"

doWithSocket :: FiniteBits a => Socket -> (a, a) -> SecureFunction -> IO [Bool]
doWithSocket soc (inputProduce, inputConsume) test = do
    let (a, b) = (finiteBitSize inputProduce, finiteBitSize inputConsume)
    let functionCircuit = createProgramCircuit test a b

    rkey <- genRootKey
    fkeystr <- genFixedKey
    let fkey = initFixedKey fkeystr
    SBS.sendAll soc fkeystr

    keyList <- mapM (const (genKeyPair rkey))
               [1 .. finiteBitSize inputProduce + finiteBitSize inputConsume]

    let (ourList, theirList) = splitAt (finiteBitSize inputProduce) keyList
    let bothList = bits2Bools inputProduce ++ bits2Bools inputConsume
    sendList soc keyList bothList
    _

doWithoutSocket :: FiniteBits a => (a, a) -> SecureFunction -> IO [Bool]
doWithoutSocket input test = do
    soc <- getSocket
    ans <- doWithSocket soc input test
    close soc
    return ans

-- where
--   wrapConstants :: Literal -> GenM Bool
--   wrapConstants Input{keym} = do
--     (Producer x0' x1') <- keym
--     let (x0, x1) = (LBS.fromStrict x0', LBS.fromStrict x1')
--     yield x0
--     yield x1
--     ans <- await
--     return $ if
--         | ans == x0 -> False
--         | ans == x1 -> True
--         | otherwise -> error "Incorrect answer found"
--   wrapConstants (Constant b) = return b

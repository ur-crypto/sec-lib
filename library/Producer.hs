{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Producer where
import           Control.Concurrent
import           Control.Monad.State.Lazy
import           Data.Bits
import qualified Data.ByteString.Lazy           as LBS
import           Debug.Trace
import           Network.Socket
import qualified Network.Socket.ByteString      as SBS
import qualified Network.Socket.ByteString.Lazy as LSBS
import           Types
import           Utils

getSocket :: IO Socket
getSocket = do
    threadDelay 10000 --for testing purposes, makes it more likely other thread will be accepting
    addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "3000")
    let serveraddr = head addrinfos
    soc <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption soc ReuseAddr 1
    connect soc (addrAddress serveraddr)
    return soc

-- doWithSocket :: FiniteBits a => Socket -> (a, a) -> SecureFunction -> IO [Bool]
-- doWithSocket soc (inputProduce, inputConsume) test =  do
--     let l = [1..finiteBitSize inputProduce + finiteBitSize inputConsume]
--     rkey <- genRootKey
--     fkeystr <- genFixedKey
--     let fkey = initFixedKey fkeystr
--     keyList <- mapM (const (genKeyPair rkey)) l
--     let (ourList, theirList) = splitAt (finiteBitSize inputProduce) keyList
--     let bothList = bits2Bools inputProduce ++ bits2Bools inputConsume

--     let select (a0, a1) b = if b then a1 else a0

--     let wrap (k0, k1) = Input [AES fkey, RAND rkey] (return $ Producer k0 k1)
--     let (ourWrappedList, theirWrappedList) = (map wrap ourList, map wrap theirList)
--     let sentKeys = LBS.fromStrict $ mconcat $ reverse $ zipWith select keyList bothList
--     let startState = mconcat [LBS.fromStrict fkeystr, sentKeys]
--     -- print startState
--     let secureFunc = test ourWrappedList theirWrappedList

--     let (finalLazyString, sendList) = processOutputs startState secureFunc []

--     LSBS.sendAll soc finalLazyString
--     sendOutputs sendList
--     where
--       sendOutputs :: [Either KeyType Bool] -> IO [Bool]
--       sendOutputs =
--           mapM sendNodes
--           where
--           sendNodes :: Either KeyType Bool -> IO Bool
--           sendNodes (Left (Producer k0 k1)) = do
--               SBS.sendAll soc k0
--               SBS.sendAll soc k1
--               -- printKey (Just False) k0
--               -- printKey (Just True) k1
--               ans <- SBS.recv soc (fromIntegral cipherSize)
--               -- printKey (Nothing) ans
--               let res = if
--                         | ans == k0 -> False
--                         | ans == k1 -> True
--                         | otherwise -> error "Incorrect answer found"
--               return res
--           sendNodes (Right k) = return k

-- doWithoutSocket :: FiniteBits a => (a, a) -> SecureFunction -> IO [Bool]
-- doWithoutSocket input test = do
--     soc <- getSocket
--     ans <- doWithSocket soc input test
--     close soc
--     return ans

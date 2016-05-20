{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Producer where
import           Control.Concurrent
import           Data.Bits
import           Network.Socket
import qualified Network.Socket.ByteString as SBS
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
doWithSocket soc (inputProduce, inputConsume) test =  do
    let l = [1..finiteBitSize inputProduce + finiteBitSize inputConsume]
    rkey <- genRootKey
    fkeystr <- genFixedKey
    -- putStr "Fkey"
    -- printKey (Just True) fkeystr
    let fkey = initFixedKey fkeystr
    SBS.sendAll soc fkeystr
    keyList <- mapM (const (genKeyPair rkey)) l
    let (ourList, theirList) = splitAt (finiteBitSize inputProduce) keyList
    let bothList = bits2Bools inputProduce ++ bits2Bools inputConsume
    -- putStrLn "Key List:"
    sendList soc keyList bothList
    -- putStrLn ""
    let wrap (k0, k1) = Input soc [AES fkey, RAND rkey] (return $ Producer k0 k1)
    let (ourWrappedList, theirWrappedList) = (map wrap ourList, map wrap theirList)
    let !res = test ourWrappedList theirWrappedList
    sendOutputs res
    where
    sendOutputs :: [Literal] -> IO [Bool]
    sendOutputs =
        mapM sendNodes
        where
        sendNodes :: Literal -> IO Bool
        sendNodes Input {value = k'} = do
            (Producer k0 k1) <- k'
            SBS.sendAll soc k0
            SBS.sendAll soc k1
            ans <- SBS.recv soc cipherSize
            return $ if
                | ans == k0 -> False
                | ans == k1 -> True
                | otherwise -> error "Incorrect answer found"
        sendNodes (Constant k) = return k

doWithoutSocket :: FiniteBits a => (a, a) -> SecureFunction -> IO [Bool]
doWithoutSocket input test = do
    soc <- getSocket
    ans <- doWithSocket soc input test
    close soc
    return ans

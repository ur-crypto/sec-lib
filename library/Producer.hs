{-# LANGUAGE MultiWayIf #-}
module Producer where
import           Control.Concurrent
import           Data.Bits
import qualified Data.ByteString           as BS
import           Gate
import           Network.Socket
import qualified Network.Socket.ByteString as SBS
import           Types
import           Utils


processGates :: Socket -> Key -> Key -> [PKey] -> IO [PKey]
processGates soc rkey fkeystr gates = do
    let fkey = initFixedKey fkeystr
    res <- mapM (process (Just soc) [AES fkey, RAND rkey]) gates
    --print res
    return res

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
    if b
        then SBS.sendAll soc kp1
        else SBS.sendAll soc kp0
    sendList soc kps bs

sendList _ _ _ = return $ error "Unbalanced send list"

doWithSocket :: FiniteBits a => Socket -> (a, a) -> SecureFunction (Key, Key) -> IO [Bool]
doWithSocket soc (inputProduce, inputConsume) test =  do
    let l = [1..((finiteBitSize inputProduce) + (finiteBitSize inputConsume))]
    rkey <- genRootKey
    fkeystr <- genFixedKey
    SBS.sendAll soc fkeystr
    keyList <- mapM (const (genKeyPair rkey)) l
    let (ourList, theirList) = splitAt (finiteBitSize inputProduce) (map Input keyList)
    let bothList = (bits2Bools inputProduce) ++ (bits2Bools inputConsume)
    sendList soc keyList bothList
    nodes <- Producer.processGates soc rkey fkeystr $ test ourList theirList
    sendOutputs nodes
    where
    sendOutputs :: [PKey] -> IO [Bool]
    sendOutputs nodes = do
        mapM sendNodes nodes
        where
        sendNodes :: PKey -> IO Bool
        sendNodes (Input (k0, k1)) = do
            SBS.sendAll soc k0
            SBS.sendAll soc k1
            ans <- SBS.recv soc cipherSize
            return $ if
                | ans == k0 -> False
                | ans == k1 -> True
                | otherwise -> error "Incorrect answer found"
        sendNodes (Constant k) = return k
        sendNodes _ =  error "Should not be sending raw values"

doWithoutSocket :: FiniteBits a => (a, a) -> SecureFunction (Key, Key) -> IO [Bool]
doWithoutSocket input test = do
    soc <- getSocket
    ans <- doWithSocket soc input test
    close soc
    return ans

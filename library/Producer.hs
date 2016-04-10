{-# LANGUAGE MultiWayIf #-}
module Producer where
import Utils
import Types
import Network.Socket
import qualified Network.Socket.ByteString as SBS
import Data.Bits
import Control.Concurrent

type PKey = Node (Key, Key)
type PTT = TruthTable (Key, Key, Key)

processGates :: Socket -> [PKey] -> IO [PKey]
processGates soc gates = mapM processGate gates
    where
    processGate :: PKey -> IO PKey
    processGate (Gate t k1 k2) = do
        a <- processGate k1
        b <- processGate k2
        ok <- genKeyPair
        let o = (Input ok)
        let tt = getTT t o a b
        sendInfo tt
        return o
        where
        getTT AND (Input (o0, o1)) = helper o0 o0 o0 o1
        getTT OR (Input (o0, o1)) = helper o0 o1 o1 o1
        getTT XOR (Input (o0, o1)) = helper o0 o1 o1 o0
        getTT NAND (Input (o0, o1)) = helper o1 o1 o1 o0
        getTT BIJ (Input (o0, o1)) = helper o1 o0 o0 o1
        getTT _ _ = error "Should not pass gates to gates"

        helper o1 o2 o3 o4 (Input (a0, a1)) (Input (b0, b1))=
            TruthTable (a0, b0, o1) (a0, b1, o2) (a1, b0, o3) (a1, b1, o4)
        helper _ _ _ _ _ _ = error "Should only pass values"
        sendInfo :: PTT -> IO()
        sendInfo (TruthTable r1 r2 r3 r4) = do
            let outkeys = map encOutKey [r1, r2, r3, r4]
            SBS.sendMany soc outkeys
    processGate x = return x

getSocket :: IO Socket
getSocket = do
    threadDelay 1000 --for testing purposes, makes it more likely other thread will be accepting
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
    keyList <- mapM (const genKeyPair) l
    let (ourList, theirList) = splitAt (finiteBitSize inputProduce) (map Input keyList)
    let bothList = (bitsToBools inputProduce) ++ (bitsToBools inputConsume)
    sendList soc keyList bothList
    nodes <- Producer.processGates soc $ test ourList theirList
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
        sendNodes _ =  error "Should not be sending raw values"

doWithoutSocket :: FiniteBits a => (a, a) -> SecureFunction (Key, Key) -> IO [Bool]
doWithoutSocket input test = do
    soc <- getSocket
    ans <- doWithSocket soc input test
    close soc
    return ans

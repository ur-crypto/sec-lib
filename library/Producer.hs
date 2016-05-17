{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Producer where
import           Control.Concurrent
import           Data.Bits
import qualified Data.ByteString           as BS
import           Data.List
import           Gate
import           Network.Socket
import qualified Network.Socket.ByteString as SBS
import           Types
import           Utils

instance LocalValue (Key, Key) where
    notHandler (Input (k1, k2)) = return $ Input (k2, k1)
    notHandler (Constant l) = return $ Constant $ not l
    notHandler _ = error "Should not recieve Gate"
    gateHandler _ _ XOR (a0, a1) (b0, b1) =
        let o1 = BS.pack $ BS.zipWith xor a0 b0
            o2 = BS.pack $ BS.zipWith xor a0 b1 in
        return $ Input (o1, o2)
    gateHandler (Just soc) [AES fkey,RAND rkey] ty a b = do
        let unsorted = getTT ty (False, True) a b
        let sorted = sortBy order unsorted
        let o = mkKeyPair sorted
        let tt = map (insertKey o) sorted
        sendInfo tt
        return $ Input o
        where
            getTT AND (o0, o1) = helper o0 o0 o0 o1
            getTT OR (o0, o1) = helper o0 o1 o1 o1
            getTT XOR (o0, o1) = helper o0 o1 o1 o0
            getTT NAND (o0, o1) = helper o1 o1 o1 o0
            getTT BIJ (o0, o1) = helper o1 o0 o0 o1
            getTT _ _ = error "Should not pass gates to gates"
            helper o1 o2 o3 o4 (a0, a1) (b0, b1)=
              [(a0, b0, o1), (a0, b1, o2), (a1, b0, o3), (a1, b1, o4)]
            helper _ _ _ _ _ _ = error "Should only pass values"
            order (p, q, _) (r, s, _) =
              let p' = fromEnum $ BS.last p
                  q' = fromEnum $ BS.last q
                  r' = fromEnum $ BS.last r
                  s' = fromEnum $ BS.last s in
                  case (compare p' r', compare q' s') of
                    (EQ, EQ) -> error "List should not contain same element"
                    (EQ, x) -> x
                    (x, _) -> x
            mkKeyPair ((x, y, oB):_) =
              let
                k' = hashPair fkey x y
                in mkKeyPairFromKey rkey k' oB
            mkKeyPair _ = error "Attempting to make key pair of empty list"
            insertKey (o0, o1) (x, y, bool) = if bool then (x, y, o1) else (x, y, o0)
            sendInfo tt = SBS.sendMany soc $ encTruthTable tt
              where
                encTruthTable = map (enc fkey)
    gateHandler Nothing _ _ _ _ = error "Needs a socket"
    gateHandler _ _ _ _ _ = error "Needs Correct Keys"

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

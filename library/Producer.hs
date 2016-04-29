{-# LANGUAGE MultiWayIf #-}
module Producer where
import Utils
import Types
import Network.Socket
import qualified Network.Socket.ByteString as SBS
import qualified Data.ByteString as BS
import Data.Bits
import Control.Concurrent

type PKey = Node (Key, Key)
type PTT = TruthTable (Key, Key, Key)

processGates :: Socket -> Key -> Key -> [PKey] -> IO [PKey]
processGates soc rkey fkeystr gates = do
    let fkey = initFixedKey fkeystr
    ret <- mapM (processGate fkey) gates
    return ret
    where
    processGate :: FixedKey -> PKey -> IO PKey
--    processGate fkey (Gate XOR (Input (a0,a1)) (Input (b0,b1))) =
--        let o1 = BS.pack $ BS.zipWith (xor) a0 b0 
--            o2 = BS.pack $ BS.zipWith (xor) a0 b1 in
--        return (Input (o1, o2))
    processGate fkey (Gate ty k1 k2) = do
        x <- processGate fkey k1
        y <- processGate fkey k2
        ret <- case (x, y) of
            ((Input a), (Input b)) -> do
                   ok <- genKeyPair rkey
                   let o = (Input ok)
                   let tt = getTT ty o (Input a) (Input b)
                   sendInfo tt
                   return o
            ((Input a), (Constant b)) ->
                processConstant ty a b
            ((Constant a), (Input b)) ->
                processConstant ty b a
            ((Constant a), (Constant b)) ->
                return $ case ty of
                    AND -> Constant (a && b)
                    OR -> Constant (a || b)
                    XOR -> Constant (xor a b)
                    NAND -> Constant (not (a && b))
                    BIJ -> Constant (not (xor a b))
            (_, _) -> error "process gate returning wrong value"
        return ret
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
            outkeys <- shuffle $ map (encOutKey fkey) [r1, r2, r3, r4]
            SBS.sendMany soc outkeys

        processConstant :: GateType -> (Key, Key) -> Bool -> IO PKey
        processConstant AND _ False = return $ Constant False
        processConstant AND key True = return $ Input key
        processConstant OR key False = return $ Input key
        processConstant OR _ True = return $ Constant True
        processConstant XOR key False = return $ Input key
        processConstant XOR key True = processGate fkey (Not (Input key))
        processConstant NAND _ False = return $ Constant True
        processConstant NAND key True = return $ Input key
        processConstant BIJ key False = processGate fkey (Not (Input key))
        processConstant BIJ key True = return $ Input key
    processGate fkey (Not node) = do
        (Input (k1, k2)) <- processGate fkey node
        return (Input (k2, k1))
    processGate _ x = return x

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
    let bothList = (bitsToBools inputProduce) ++ (bitsToBools inputConsume)
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

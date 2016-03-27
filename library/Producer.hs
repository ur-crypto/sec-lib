module Producer where
import Utils
import Types
import Network.Socket
import qualified Network.Socket.ByteString as SBS
import Data.Bits

type PKey = Value (Key, Key)
type PTT = TruthTable (Key, Key, Key)

sendInfo :: Socket -> PTT -> IO()
sendInfo soc (TruthTable r1 r2 r3 r4) = do
    let outkeys = map encOutKey [r1, r2, r3, r4]
    SBS.sendMany soc outkeys

processGate :: Socket -> PKey -> IO PKey
processGate soc (Gate t k1 k2) = do
    a <- processGate soc k1
    b <- processGate soc k2
    ok <- genKeyPair
    let o = (Input ok)
    let tt = getTT t o a b
    sendInfo soc tt
    return o
    where
    helper o1 o2 o3 o4 (Input (a0, a1)) (Input (b0, b1))=
        TruthTable (a0, b0, o1) (a0, b1, o2) (a1, b0, o3) (a1, b1, o4)
    helper _ _ _ _ _ _ = error "Should only pass values"
    getTT AND (Input (o0, o1)) = helper o0 o0 o0 o1
    getTT OR (Input (o0, o1)) = helper o0 o1 o1 o1
    getTT XOR (Input (o0, o1)) = helper o0 o1 o1 o0
    getTT NAND (Input (o0, o1)) = helper o1 o1 o1 o0
    getTT BIJ (Input (o0, o1)) = helper o1 o0 o0 o1
    getTT _ _ = error "Should not pass gates to gates"

processGate _ (Input a) = return (Input a)

producerGetSocket :: IO Socket
producerGetSocket = do
    addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "3000")
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

--currently no OT takes place...
produceMain :: (Int, Int) -> TestBool (Key, Key) -> IO (Key, Key)
produceMain (inputProduce, inputConsume) test = do
    keyList <- mapM (const genKeyPair) [1..((finiteBitSize inputProduce) + (finiteBitSize inputConsume))]
    let (ourList, theirList) = splitAt (finiteBitSize inputProduce) (map Input keyList)
    let bothList = (bitsToBools inputProduce) ++ (bitsToBools inputConsume)
    soc <- producerGetSocket 
    sendList soc keyList bothList
    (Input (o0, o1)) <- Producer.processGate soc $ test ourList theirList
    close soc
    return (o0, o1)

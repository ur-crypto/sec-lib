import Prelude
import Producer
import Network.Socket
import Network.Socket.ByteString as SBS
import Utils
import Types
import Examples
import Data.Bits

testList :: [Bool]
testList = bitsToBools testNum

getSocket :: IO Socket
getSocket = do
    addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "3000")
    let serveraddr = head addrinfos
    soc <- socket (addrFamily serveraddr) Stream defaultProtocol
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

main :: IO ()
main = do
    keyList <- mapM (const genKeyPair) [1..(2*(finiteBitSize testNum))]

    -- mapM (zipWith const (cycle [Just False, Just True]) [1..

    let (ourList, theirList) = splitAt (finiteBitSize testNum) (map Input keyList)
    let bothList = testList ++ testList
    soc <- getSocket 
    sendList soc keyList bothList
    (Input (o0, o1)) <- processGate soc $ numCmp ourList theirList
    putStrLn "Value : Output Key"
    printKey (Just False) o0
    printKey (Just True) o1
    return ()

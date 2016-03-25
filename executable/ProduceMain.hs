import Prelude
import Producer
import Network.Socket
import Network.Socket.ByteString as SBS
import Utils
import Types

testList :: [Bool]
testList = [True, False, True, True, False, True, True, False,
            True, False, True, True, False, True, True, False]

wordCmp :: [KeyPair] -> [KeyPair] -> KeyPair
wordCmp n1 n2 = foldl1 (.&.) (zipWith bij n1 n2)

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
    keyList <- mapM (const genKeyPair) [1..32 :: Integer]
    let (ourList, theirList) = splitAt 16 (map Input keyList)
    putStrLn "Start Keys"
    let boolList = testList ++ testList
    putStrLn ""
    soc <- getSocket 
    sendList soc keyList boolList
    (Input (o0, o1)) <- processGate soc $ wordCmp ourList theirList
    printKey (Just False) o0
    printKey (Just True) o1

    
    return ()

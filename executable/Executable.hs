import Prelude 
import qualified Consumer
import qualified Producer
import Network.Socket
import Network.Socket.ByteString as SBS
import Utils
import Types
import Examples
import Data.Bits
import System.Environment

consumeGetSocket :: IO Socket
consumeGetSocket = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "3000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 1
    (conn, _) <- accept sock
    return conn

receiveList :: Socket -> Int -> IO [Key]
receiveList soc num = mapM (const $ SBS.recv soc cipherSize) [1..num]

consumeMain :: IO ()
consumeMain = do
    soc <- consumeGetSocket
    keyList <- receiveList soc (2 * (finiteBitSize testNum)) 
    let (theirList, ourList) = splitAt (finiteBitSize testNum) $ map Input keyList
    (Input o) <- Consumer.processGate soc $ numCmp theirList ourList
    putStrLn "Answer"
    printKey Nothing o
    return ()

testList :: [Bool]
testList = bitsToBools testNum

producerGetSocket :: IO Socket
producerGetSocket = do
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

produceMain :: IO ()
produceMain = do
    keyList <- mapM (const genKeyPair) [1..(2*(finiteBitSize testNum))]
    let (ourList, theirList) = splitAt (finiteBitSize testNum) (map Input keyList)
    let bothList = testList ++ testList
    soc <- producerGetSocket 
    sendList soc keyList bothList
    (Input (o0, o1)) <- Producer.processGate soc $ numCmp ourList theirList
    putStrLn "Value : Output Key"
    printKey (Just False) o0
    printKey (Just True) o1
    return ()
    
data Mode = Producer | Consumer

usage :: IO()
usage = putStrLn "Enter producer or consumer"


parseArgs :: [String] -> Maybe Mode
parseArgs ("producer":_) = Just Producer
parseArgs ("consumer":_) = Just Consumer
parseArgs [] = Nothing
parseArgs _ = Nothing

main :: IO()
main = do
    args <- getArgs
    case (parseArgs args) of
        Just Producer -> produceMain
        Just Consumer -> consumeMain
        Nothing -> usage
        

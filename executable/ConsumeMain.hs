import Prelude 
import Consumer
import Network.Socket
import Network.Socket.ByteString as SBS
import Utils
import Types

wordCmp :: [VKey] -> [VKey] -> VKey
wordCmp n1 n2 = foldl1 (.&.) (zipWith  bij n1 n2)

getSocket :: IO Socket
getSocket = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "3000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 1
    (conn, _) <- accept sock
    return conn

receiveList :: Socket -> Integer -> IO [Key]
receiveList soc num = mapM (const $ SBS.recv soc cipherSize) [1..num]

main :: IO ()
main = do
    soc <- getSocket
    keyList <- receiveList soc 32
    let (theirList, ourList) = splitAt 16 $ map Input keyList
    putStrLn "Starting Keys (from OT)"
    
    (Input o) <- processGate soc $ wordCmp theirList ourList
    putStrLn ""
    putStrLn "Answer"
    printKey Nothing o
    return ()

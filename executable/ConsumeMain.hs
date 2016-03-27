import Prelude 
import Consumer
import Network.Socket
import Network.Socket.ByteString as SBS
import Utils
import Types
import Examples
import Data.Bits

getSocket :: IO Socket
getSocket = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "3000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 1
    (conn, _) <- accept sock
    return conn

receiveList :: Socket -> Int -> IO [Key]
receiveList soc num = mapM (const $ SBS.recv soc cipherSize) [1..num]

main :: IO ()
main = do
    soc <- getSocket
    keyList <- receiveList soc (2 * (finiteBitSize testNum)) 
    let (theirList, ourList) = splitAt (finiteBitSize testNum) $ map Input keyList
    (Input o) <- processGate soc $ numCmp theirList ourList
    putStrLn "Answer"
    printKey Nothing o
    return ()

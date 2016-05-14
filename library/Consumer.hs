{-# LANGUAGE MultiWayIf #-}
module Consumer where
import           Data.Bits
import qualified Data.ByteString           as B
import           Gate
import           Network.Socket
import qualified Network.Socket.ByteString as SBS
import           Types
import           Utils

processGates :: Socket -> Key -> [CKey] -> IO [CKey]
processGates soc fkeystr gates = do
    let fkey = initFixedKey fkeystr
    mapM (process (Just soc) [AES fkey]) gates

getSocket :: IO Socket
getSocket = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "3000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress serveraddr)
    listen sock 1
    (conn, _) <- accept sock
    return conn

doWithSocket :: FiniteBits a => Socket -> (a, a) -> SecureFunction Key -> IO [Bool]
doWithSocket soc (produceInput, consumeInput) test = do
    let l = ((finiteBitSize produceInput) + (finiteBitSize consumeInput))
    fkeystr <- SBS.recv soc cipherSize
    keyList <- receiveList l
    let (theirList, ourList) = splitAt (finiteBitSize produceInput) $ map Input keyList
    nodes <- Consumer.processGates soc fkeystr $ test theirList ourList
    receiveOutputs nodes
    where
    receiveList :: Int -> IO [Key]
    receiveList num = mapM (const $ SBS.recv soc cipherSize) [1..num]
    receiveOutputs :: [CKey] -> IO [Bool]
    receiveOutputs = mapM receiveNodes
        where
        receiveNodes :: Node Key -> IO Bool
        receiveNodes (Input k) = do
            o0 <- SBS.recv soc cipherSize
            o1 <- SBS.recv soc cipherSize
            SBS.sendAll soc k
            return $ if
                | k == o0 -> False
                | k == o1 -> True
                | otherwise -> error "Incorrect answer found"
        receiveNodes (Constant k) = return k
        receiveNodes (Not k) = receiveNodes k
        receiveNodes x = error "Should not be receiving gate"

doWithoutSocket ::FiniteBits a => (a, a) -> SecureFunction Key -> IO [Bool]
doWithoutSocket input test = do
    soc <- getSocket
    ans <- doWithSocket soc input test
    close soc
    return ans

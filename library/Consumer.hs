module Consumer where
import Utils
import Types
import Network.Socket
import Data.Bits
import qualified Network.Socket.ByteString as SBS
import qualified Data.ByteString as B


type CKey = Value Key
type CTT = TruthTable Key

getTT :: Socket -> IO CTT
getTT soc = do
    byteTT <- SBS.recv soc (4 * cipherSize)
    let (x1, r1) = B.splitAt cipherSize byteTT
    let (x2, r2) = B.splitAt cipherSize r1
    let (x3, x4) = B.splitAt cipherSize r2
    return $ TruthTable x1 x2 x3 x4

processTT :: CTT -> CKey -> CKey -> CKey
processTT (TruthTable a b c d) (Input k1) (Input k2) =
    let (Just k) = head $ filter corrKey $ map decOutKey keyList in
    Input k
    where
    corrKey :: Maybe Key -> Bool
    corrKey (Just _) = True
    corrKey Nothing = False
    keyList =  [(k1, k2, a), (k1, k2, b), (k1, k2, c), (k1, k2, d)] 
processTT _ _ _ = error "Passing gate to processTT"

processGate :: Socket -> CKey -> IO CKey
processGate soc (Gate _ k1 k2) = do
    a <- processGate soc k1
    b <- processGate soc k2
    tt <- getTT soc
    return $ processTT tt a b

processGate _ (Input a) = return (Input a)

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

receiveList :: Socket -> Int -> IO [Key]
receiveList soc num = mapM (const $ SBS.recv soc cipherSize) [1..num]

doWithSocket :: FiniteBits a => Socket -> (a, a) -> TestBool Key -> IO B.ByteString
doWithSocket soc (produceInput, consumeInput) test = do
    keyList <- receiveList soc ((finiteBitSize produceInput) + (finiteBitSize consumeInput))
    let (theirList, ourList) = splitAt (finiteBitSize produceInput) $ map Input keyList
    (Input o) <- Consumer.processGate soc $ test theirList ourList
    return o

doWithoutSocket ::FiniteBits a => (a, a) -> TestBool Key -> IO B.ByteString
doWithoutSocket input test = do
    soc <- getSocket
    o <- doWithSocket soc input test
    close soc
    return o

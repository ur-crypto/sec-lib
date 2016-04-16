{-# LANGUAGE MultiWayIf #-}
module Consumer where
import Utils
import Types
import Network.Socket
import Data.Bits
import qualified Network.Socket.ByteString as SBS
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed.Mutable as VM

type CKey = Node Key
type CTT = TruthTable Key

processGates :: Socket -> [CKey] -> IO [CKey]
processGates soc gates = do
    gateTypes <- VM.new 6 
    vals <- mapM (processGate gateTypes) gates
    _ <- mapM (printGates gateTypes) [0 .. 5]
    return vals
    where
    processGate :: VM.IOVector Int -> CKey -> IO CKey
    processGate vec (Gate ty k1 k2) = do
        x <- processGate vec k1
        y <- processGate vec k2
        ret <- case (x, y) of
            ((Input a), (Input b)) -> do
                countType vec ty
                tt <- getTT
                return $ processTT tt (Input a) (Input b)
            ((Input a), (Constant b)) ->
                processConstant vec ty a b
            ((Constant a), (Input b)) ->
                processConstant vec ty b a
            ((Constant a), (Constant b)) ->
                return $ case ty of
                    AND -> Constant (a && b)
                    OR -> Constant (a || b)
                    XOR -> Constant (xor a b)
                    NAND -> Constant (not (a && b))
                    BIJ -> Constant (not (xor a b))
            (_, _) -> error "process gate returning wrong value"
        return ret
    processGate vec (Not a) = do
        VM.modify vec (\x -> x+1) 5
        return a
    processGate _ x = return x
    countType vec ty = case ty of
                    AND     -> VM.modify vec (\x -> x+1) 0 
                    OR      -> VM.modify vec (\x -> x+1) 1 
                    XOR     -> VM.modify vec (\x -> x+1) 2 
                    NAND    -> VM.modify vec (\x -> x+1) 3 
                    BIJ     -> VM.modify vec (\x -> x+1) 4 
    processConstant :: VM.IOVector Int -> GateType -> Key -> Bool -> IO CKey
    processConstant _ AND _ False = return $ Constant False
    processConstant _ AND key True = return $ Input key
    processConstant _ OR key False = return $ Input key
    processConstant _ OR _ True = return $ Constant True
    processConstant _ XOR key False = return $ Input key
    processConstant vec XOR key True = processGate vec (Not (Input key))
    processConstant _ NAND _ False = return $ Constant True
    processConstant _ NAND key True = return $ Input key
    processConstant vec BIJ key False = processGate vec (Not (Input key))
    processConstant _ BIJ key True = return $ Input key
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
    getTT :: IO CTT
    getTT = do
        byteTT <- SBS.recv soc (4 * cipherSize)
        let (x1, r1) = B.splitAt cipherSize byteTT
        let (x2, r2) = B.splitAt cipherSize r1
        let (x3, x4) = B.splitAt cipherSize r2
        return $ TruthTable x1 x2 x3 x4
    printGates vec i = do
        let ty = if | i == 0 -> "AND: " | i == 1 -> "OR: " | i == 2 -> "XOR: "
                    | i == 3 -> "NAND: " | i == 4 -> "BIJ: " | i == 5 -> "NOT: "
        num <- VM.read vec i
        putStrLn (ty ++ (show num))

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
    keyList <- receiveList l
    let (theirList, ourList) = splitAt (finiteBitSize produceInput) $ map Input keyList
    nodes <- Consumer.processGates soc $ test theirList ourList
    receiveOutputs nodes
    where
    receiveList :: Int -> IO [Key]
    receiveList num = mapM (const $ SBS.recv soc cipherSize) [1..num]
    receiveOutputs :: [CKey] -> IO [Bool]
    receiveOutputs nodes = mapM receiveNodes nodes
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
        receiveNodes _ = error "Don't pass completed list"

doWithoutSocket ::FiniteBits a => (a, a) -> SecureFunction Key -> IO [Bool]
doWithoutSocket input test = do
    soc <- getSocket
    ans <- doWithSocket soc input test
    close soc
    return ans


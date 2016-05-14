{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Gate where
import           Data.Bits
import qualified Data.ByteString           as BS
import           Network.Socket
import qualified Network.Socket.ByteString as SBS
import           Types
import           Utils

data KeyContext = AES FixedKey
                | RAND Key

type GateCounter = [Int]

class LocalValue v where
    gateHandler :: Maybe Socket -> [KeyContext] -> GateType -> v -> v -> IO (Node v)
    notHandler :: Node v -> IO (Node v)
    process :: Maybe Socket -> [KeyContext] -> Node v -> IO (Node v)
    process soc fkeys (Gate ty k1 k2) = do
        !x <- process soc fkeys k1
        !y <- process soc fkeys k2
        case (x, y) of
            (Input a, Input b) ->
                gateHandler soc fkeys ty a b
            (Input _, Constant b) ->
                processConstant ty x b
            (Constant a, Input _) ->
                processConstant ty y a
            (Constant a, Constant b) ->
                return $ case ty of
                    AND -> Constant (a && b)
                    OR -> Constant (a || b)
                    XOR -> Constant (xor a b)
                    NAND -> Constant (not (a && b))
                    BIJ -> Constant (not (xor a b))
            (_, _) -> error "process gate returning wrong value"
        where
        processConstant AND _ False = return $ Constant False
        processConstant AND key True = return key
        processConstant OR key False = return key
        processConstant OR _ True = return $ Constant True
        processConstant XOR key False = return key
        processConstant XOR key True = process soc fkeys (Not key)
        processConstant NAND _ False = return $ Constant True
        processConstant NAND key True = return key
        processConstant BIJ key False = process soc fkeys (Not key)
        processConstant BIJ key True = return key
    process soc fkeys (Not node) = do
        procNode <- process soc fkeys node
        notHandler procNode
    process _ _ x = return x

instance LocalValue (Key, Key) where
    notHandler (Input (k1, k2)) = return $ Input (k2, k1)
    notHandler (Constant l) = return $ Constant $ not l
    notHandler _ = error "Should not recieve Gate"
    gateHandler _ _ XOR (a0, _) (b0, b1) =
        let o1 = BS.pack $ BS.zipWith xor a0 b0
            o2 = BS.pack $ BS.zipWith xor a0 b1 in
        return (Input (o1, o2))
    gateHandler (Just soc) [AES fkey,RAND rkey] ty a b = do
        ok <- genKeyPair rkey
        let o = Input ok
        let tt = getTT ty o (Input a) (Input b)
        sendInfo tt
        return o
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
            sendInfo tt = SBS.sendMany soc $ encTruthTable fkey tt
    gateHandler Nothing _ _ _ _ = error "Needs a socket"
    gateHandler _ _ _ _ _ = error "Needs Correct Keys"

instance LocalValue Key where
    notHandler (Input a) = return $ Input a
    notHandler (Constant l) = return $ Constant $ not l
    notHandler _ = error "Should not recieve Gate"
    gateHandler _ _ XOR a b =
        return $ Input $ BS.pack $ BS.zipWith xor a b
    gateHandler (Just soc) [AES fkey] _ x y = do
        tt <- getTT
        return $ Input $ decTruthTable fkey x y tt
        where
            getTT = do
                byteTT <- SBS.recv soc (4 * cipherSize)
                let (x1, r1) = BS.splitAt cipherSize byteTT
                let (x2, r2) = BS.splitAt cipherSize r1
                let (x3, x4) = BS.splitAt cipherSize r2
                return $ TruthTable x1 x2 x3 x4
    gateHandler Nothing _ _ _ _ = error "Needs a socket"
    gateHandler _ _ _ _ _ = error "Needs Correct Keys"

incCounter :: Int -> GateCounter -> GateCounter
incCounter i =
    zipWith (\ind val -> if ind == i then val + 1 else val) [0..]
instance LocalValue GateCounter where
    notHandler (Input gc) = return $ Input $ incCounter 5 gc
    notHandler other = return other
    gateHandler Nothing _ ty gc1 gc2 =
        let gc = zipWith (+) gc1 gc2 in
        return $ Input $ incCounter (type2Int ty) gc
        where
            type2Int AND = 0
            type2Int OR = 1
            type2Int XOR = 2
            type2Int BIJ = 3
            type2Int NAND = 4
    gateHandler _ _ _ _ _ = error "Does not use socket"

instance LocalValue Int where
    notHandler (Input i) = return $ Input $ i+1
    notHandler other = return other
    gateHandler Nothing _ _ ai bi = return $ Input $ ai + bi + 1
    gateHandler _ _ _ _ _ = error "Does not use socket"

countGates :: Int -> SecureFunction Int -> IO()
countGates inputs func = do
    let inp = map (const $ Input 0) [0..inputs-1]
    let tree = func inp inp
    res <- process Nothing [] $ head tree
    print res

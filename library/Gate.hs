{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Gate where
import           Data.Bits
import qualified Data.ByteString           as BS
import           Data.List
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
    processConstant :: GateType -> Node v -> Node v -> Node v
    processConstant ty (Constant a) (Constant b) =
        Constant  $ case ty of
            AND -> a && b
            OR -> a || b
            XOR -> if a then not b else b
            BIJ -> not $ if a then not b else b
            NAND -> not $ (&&) a b
    processConstant ty a (Constant b) =
        case ty of
            AND -> if b then a else Constant False
            OR -> if b then Constant True else a
            XOR -> if b then Not a else a
            BIJ -> if b then a else Not a
            NAND -> if b then Not a else Constant True
    processConstant ty (Constant b) a = processConstant ty a (Constant b)
    processConstant ty a b = Gate ty a b
    process :: Maybe Socket -> [KeyContext] -> Node v -> IO (Node v)
    process soc fkeys (Gate ty !k1 !k2) = do
        (Input x) <- process soc fkeys k1
        (Input y) <- process soc fkeys k2
        gateHandler soc fkeys ty x y
    process soc fkeys (Not node) = do
        procNode <- process soc fkeys node
        notHandler procNode
    process _ _ x = return x

instance LocalValue (Key, Key) where
    notHandler (Input (k1, k2)) = return $ Input (k2, k1)
    notHandler (Constant l) = return $ Constant $ not l
    notHandler _ = error "Should not recieve Gate"
    gateHandler _ _ XOR (a0, a1) (b0, b1) =
        let o1 = BS.pack $ BS.zipWith xor a0 b0
            o2 = BS.pack $ BS.zipWith xor a0 b1 in
        return (Input (o1, o2))
    gateHandler (Just soc) [AES fkey,RAND rkey] ty a b = do
        ok <- genKeyPair rkey
        let o = (Input ok)
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
            sendInfo (TruthTable r1 r2 r3 r4) = do
                outkeys <- shuffle $ map (encOutKey fkey) [r1, r2, r3, r4]
                SBS.sendMany soc outkeys
    gateHandler Nothing _ _ _ _ = error "Needs a socket"

instance LocalValue Key where
    notHandler (Input a) = return $ Input a
    notHandler (Constant l) = return $ Constant $ not l
    notHandler _ = error "Should not recieve Gate"
    gateHandler _ _ XOR a b =
        return $ Input $ BS.pack $ BS.zipWith xor a b
    gateHandler (Just soc) [AES fkey] _ x y = do
        tt <- getTT
        return $ processTT tt x y
        where
            getTT = do
                byteTT <- SBS.recv soc (4 * cipherSize)
                let (x1, r1) = BS.splitAt cipherSize byteTT
                let (x2, r2) = BS.splitAt cipherSize r1
                let (x3, x4) = BS.splitAt cipherSize r2
                return $ TruthTable x1 x2 x3 x4
            processTT (TruthTable a b c d) k1 k2 =
                let (Just k) = head $ filter corrKey $ map (decOutKey fkey) keyList in
                Input k
                where
                corrKey :: Maybe Key -> Bool
                corrKey (Just _) = True
                corrKey Nothing = False
                keyList =  [(k1, k2, a), (k1, k2, b), (k1, k2, c), (k1, k2, d)]
            processTT  _ _ _ = error "Passing gate to processTT"
    gateHandler Nothing _ _ _ _ = error "Needs a socket"

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

instance LocalValue Int where
    notHandler (Input i) = return $ Input $ i+1
    notHandler other = return other
    gateHandler Nothing _ _ ai bi = return $ Input $ ai + bi + 1

countGates :: Int -> SecureFunction GateCounter -> IO()
countGates inputs func = do
    let inp = map (const $ Input [0,0,0,0,0,0]) [0..inputs-1]
    let tree = func inp inp
    res <- mapM (process Nothing []) tree
    print $ foldl' combine [0,0,0,0,0,0] res
        where
            combine acc (Input a) = zipWith (+) acc a
            combine acc _ = acc

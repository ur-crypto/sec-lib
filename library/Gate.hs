{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Gate where
import           Data.Bits
import           Network.Socket
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

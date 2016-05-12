{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import Data.Bits
import Data.ByteString as BS
import qualified Prelude as P

type Key = BS.ByteString
type CKey = Node Key
type CTT = TruthTable Key
type PKey = Node (Key, Key)
type PTT = TruthTable (Key, Key, Key)

type SecureFunction a = SecureNum a -> SecureNum a -> SecureNum a

data GateType   = AND
                | OR
                | XOR
                | NAND
                | BIJ deriving(P.Show)

data Node a    = Gate GateType (Node a) (Node a)
                | Not (Node a)
                | Constant P.Bool
                | Input a deriving(P.Show)

type SecureNum a = [Node a]

data TruthTable a = TruthTable a a a a

processConstant :: GateType -> Node a -> Node a -> Node a
processConstant ty (Constant a) (Constant b) =
    Constant P.$ case ty of
        AND -> a P.&& b
        OR -> a P.|| b
        XOR -> if a then P.not b else b
        BIJ -> P.not P.$ if a then P.not b else b
        NAND -> P.not P.$ (P.&&) a b
processConstant ty a (Constant b) =
    case ty of
        AND -> if b then a else Constant P.False
        OR -> if b then Constant P.True else a
        XOR -> if b then Not a else a
        BIJ -> if b then a else Not a
        NAND -> if b then Not a else Constant P.True
processConstant ty (Constant b) a = processConstant ty a (Constant b)
processConstant ty a b = Gate ty a b

instance P.Eq (Node a) where
    (==) = P.undefined
    (/=) = P.undefined

instance Bits (SecureNum a) where
    (.&.) = P.zipWith P.$ processConstant AND
    (.|.) = P.zipWith P.$ processConstant OR
    xor = P.zipWith P.$ processConstant XOR
    complement = P.map P.$ Not
    shiftL xs num = (P.drop num xs) P.++ P.map (P.const P.$ Constant P.False) [0 .. num P.-1]
    shiftR xs num = P.map (P.const P.$ Constant P.False) [0 .. num P.-1] P.++ (P.take num xs)
--    rotate x st = P.take (P.length st) P.$ P.drop (P.negate x `P.mod` P.length st) P.$ P.cycle st
    isSigned a = P.False
    testBit = P.undefined
    bit a = P.undefined
    popCount = P.undefined


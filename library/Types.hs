{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types where

import           Data.Bits
import           Data.ByteString as BS
import qualified Prelude         as P

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

data TruthTable a = TruthTable a a a a deriving(P.Show)

instance P.Eq (Node a) where
    (==) = P.undefined
    (/=) = P.undefined

instance Bits (SecureNum a) where
    (.&.) = P.zipWith P.$ Gate AND
    (.|.) = P.zipWith P.$ Gate OR
    xor = P.zipWith P.$ Gate XOR
    complement = P.map P.$ Not
    shiftL xs num = P.drop num xs P.++ P.map (P.const P.$ Constant P.False) [0 .. num P.-1]
    shiftR xs num = P.map (P.const P.$ Constant P.False) [0 .. num P.-1] P.++ P.take num xs
    rotate = P.undefined
    -- rotate x st = P.take (P.length st) P.$ P.drop (P.negate x `P.mod` P.length st) P.$ P.cycle st
    isSigned _ = P.False
    testBit = P.undefined
    bit _ = P.undefined
    popCount = P.undefined
    bitSize = P.length
    bitSizeMaybe a = P.Just P.$ P.length a

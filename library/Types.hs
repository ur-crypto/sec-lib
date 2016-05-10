{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import Data.Bits
import Data.ByteString as BS
import qualified Prelude as P

type Key = BS.ByteString

type SecureFunction a = SecureList a -> SecureList a -> SecureList a

data GateType   = AND
                | OR
                | XOR
                | NAND
                | BIJ deriving(P.Show)

data Node a    = Gate GateType (Node a) (Node a)
                | Not (Node a)
                | Constant P.Bool
                | Input a deriving(P.Show)

type SecureList a = [Node a]

data TruthTable a = TruthTable a a a a

instance P.Eq (Node a) where
    (==) = P.undefined
    (/=) = P.undefined

instance Bits (SecureList a) where
    (.&.) = P.zipWith P.$ Gate AND
    (.|.) = P.zipWith P.$ Gate OR
    xor = P.zipWith P.$ Gate XOR
    complement = P.map P.$ Not
    shiftL xs num = (P.drop num xs) P.++ P.map (P.const P.$ Constant P.False) [0 .. num P.-1]
    shiftR xs num = P.map (P.const P.$ Constant P.False) [0 .. num P.-1] P.++ (P.take num xs)
    rotate xs n = P.zipWith P.const (P.drop n (P.cycle xs)) xs


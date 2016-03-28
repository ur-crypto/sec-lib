module Examples where
import Types
import Utils
import Data.Int


test8 :: Int8
test8 = 5

test16 :: Int16
test16 = 124

test32 :: Int32
test32 = 1234567890

test64 :: Int64
test64 = 1234567890

numCmp :: TestBool a
numCmp n1 n2 = foldl1 (.&.) (zipWith bij n1 n2)

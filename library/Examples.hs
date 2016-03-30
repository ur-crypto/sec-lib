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

testb64 :: Int64
testb64 = 111111

numGT :: TestBool a
numGT [n1] [n2] = 
       let nbool = Gate NAND n2 n2 in
       n1 .&. nbool
numGT (n1:n1s) (n2:n2s) = 
       let nbool = Gate NAND n2 n2 
           mbool = Gate NAND n1 n1 in
       ifThenElse ( n1 .&. nbool) n1 (ifThenElse (mbool .&. n2) n2 (numGT n1s n2s))

numCmp :: TestBool a
numCmp n1 n2 = foldl1 (.&.) (zipWith bij n1 n2)

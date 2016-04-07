module Examples where
import Types
import Utils
import Data.Int
import Prelude hiding ((&&), (||))


test8 :: Int8
test8 = 5

test16 :: Int16
test16 = 124

test32 :: Int32
test32 = 1234567890

testb64 :: Int64
testb64 = 64 

test64 :: Int64
test64 = 64

caddInt [n1] [n2] = (n1 .&. n2)

caddInt (n1:n1s) (n2:n2s) = 
       let xsum = Gate XOR n1 n2
           asum = (n1 .&. n2)
           x = caddInt n1s n2s in
       (asum .|. (xsum .&. x)) 



addInt [n1] [n2] =
       let xsum = Gate XOR n1 n2
           asum = (n1 .&. n2) in
       (asum:xsum:[])
addInt (n1:n1s) (n2:n2s) = 
       let xsum = Gate XOR n1 n2
           asum = (n1 .&. n2)
           x = head(addInt n1s n2s)
           xs = tail (addInt n1s n2s) in
       (((asum .|. (xsum .&. x)):(Gate XOR xsum x):[])++xs) 

numPltC :: TestBool a
numPltC [n1] [n2] = 
       let nbool = Gate NAND n2 n2 in
       n1 && nbool
numPltC (n1:n1s) (n2:n2s) =
       let nbool = Gate NAND n2 n2 
           mbool = Gate NAND n1 n1 in
       ifThenElse ( n1 && nbool) n1 (ifThenElse (mbool && n2) n1 (numPltC n1s n2s))

numCmp :: TestBool a
numCmp n1 n2 = foldl1 (&&) (zipWith bij n1 n2)

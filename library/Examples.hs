module Examples where
import Types
import Ops
import Data.Int
import Prelude hiding ((&&), (||), ifThenElse)


test8 :: Int8
test8 = 5

test16 :: Int16
test16 = 124

test32 :: Int32
test32 = 1234567890

testb64 :: Int64
testb64 = 100 

test64 :: Int64
test64 = 164

caddInt [n1] [n2] = (n1 && n2)

caddInt (n1:n1s) (n2:n2s) = 
       let xsum = Gate XOR n1 n2
           asum = (n1 && n2)
           x = caddInt n1s n2s in
       (asum || (xsum && x)) 



addInt [n1] [n2] =
       let xsum = Gate XOR n1 n2
           asum = (n1 && n2) in
       (asum:xsum:[])
addInt (n1:n1s) (n2:n2s) = 
       let xsum = Gate XOR n1 n2
           asum = (n1 && n2)
           x = head(addInt n1s n2s)
           xs = tail (addInt n1s n2s) in
       (((asum || (xsum && x)):(Gate XOR xsum x):[])++xs) 

numCmp :: SecureFunction a
numCmp as bs = [imp as bs]
    where
    imp :: [Node a] -> [Node a] -> Node a
    imp [n1] [n2] = 
           let nbool = nand n2 n2 in
           n1 && nbool
    imp (n1:n1s) (n2:n2s) =
           let nbool = nand n2 n2 
               mbool = nand n1 n1 in
           ifThenElse ( n1 && nbool) n1 (ifThenElse (mbool && n2) n1 (imp n1s n2s))
    imp _ _ = error "Bad args for imp"

numEq :: SecureFunction a
numEq n1 n2 = [foldl1 (&&) (zipWith bij n1 n2)]

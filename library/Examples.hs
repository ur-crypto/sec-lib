module Examples where
import Types
import Ops
import Data.Int
import Prelude hiding ((&&), (||), ifThenElse)


test8 :: Int8
test8 = 5

test16 :: Int16
test16 = 125

testb16 :: Int16
testb16 = 200

test32 :: Int32
test32 = 39393 


testb32 :: Int32
testb32 = 120200

testb64 :: Int64
testb64 = 2384 

test64 :: Int64
test64 = 5345

testb8 :: Int8
testb8 = 12


caddInt [n1] [n2] = (n1 && n2)

caddInt (n1:n1s) (n2:n2s) = 
       let xsum = Gate XOR n1 n2
           asum = (n1 && n2)
           x = caddInt n1s n2s in
       (asum || (xsum && x)) 

lbij n1 n2 = let result = bij n1 n2 in ((Gate NAND result result):[])


--addTwice [n1] [n2] = (n1:n2:[])
addTwice n1 n2 = addInt n1 (addInt n1 n2)

addIntF :: Int -> [Node a1] -> Int -> [Node a1] -> (Int, (Node a1, [Node a1]))
addIntF m1 [n1] m2 [n2] = if (m1==1) then 
                                     if (m2==1) then
                                                (2,(n1 && n2,((Gate XOR n1 n2):[])))
                                     else (2,(n1 && n2,((Gate XOR n1 n2):[])))
                          else (2,(n1 && n2,((Gate XOR n1 n2):[])))

addIntF m1 (n1:n1s) m2 (n2:n2s) = 
  let cond1 = (m1 > 5) 
      cond2 = (m2 > 5)
      cond3 = (m1 > m2)
      cond4 = (m1 < m2) in
      case (cond1,cond2,cond3,cond4) of
          (True,True,_,_)             -> addIntF (m1-1) n1s (m2-1) n2s 
          (True,False,_,_)            -> addIntF (m1-1) n1s m2 (n2:n2s) 
          (False,True,_,_)            -> addIntF m1 (n1:n1s) (m2-1) n2s 
          (False,False,False,False)   -> let (len,(carry,remsum)) = addIntF (m1-1) n1s (m2-1) n2s in
                                         (len+1,(((carry && n1) || (carry && n2) || (n1&&n2)),((Gate XOR n1 (Gate XOR n2 carry)):[])++remsum))
          (False,False,True,_)        -> let (len,(carry,remsum)) = addIntF (m1-1) n1s m2 (n2:n2s) in
                                         (len+1,((carry && n1),((Gate XOR n1 carry):[])++remsum))
          (False,False,False,True)    -> let (len,(carry,remsum)) = addIntF m1 (n1:n1s) (m2-1) n2s in
                                         (len+1,((carry && n2),((Gate XOR n2 carry):[])++remsum)) 
addInt [n1] [n2] =
       let xsum = Gate XOR n1 n2
           asum = (n1 && n2) in
       (asum:xsum:[])
addInt (n1:n1s) (n2:n2s) = 
             if (length n1s) == (length n2s)
             then let xsum = Gate XOR n1 n2
                      asum = (n1 && n2)
                      retlist = (addInt n1s n2s) in
                         let x = head(retlist) 
                             xs = tail (retlist) in
                             (((asum || (xsum && x)):(Gate XOR xsum x):[])++xs) 
             else if (length n1s) < (length n2s) 
                  then let retlist = addInt (n1:n1s) n2s in
                           let x = head(retlist)
                               xs = tail (retlist) in 
                               (((n2 && x):(Gate XOR n2 x):[])++xs) 
                  else let retlist = addInt n1s (n2:n2s) in
                           let x = head(retlist) 
                               xs = tail (retlist) in
                               (((n1 && x):(Gate XOR n1 x):[])++xs)

numCmp :: SecureFunction a
numCmp as bs = [imp as bs]
    where
    imp :: [Node a] -> [Node a] -> Node a
    imp [n1] [n2] = 
           let nbool = Gate NAND n2 n2 in
           n1 && nbool
    imp (n1:n1s) (n2:n2s) =
           let nbool = Gate NAND n2 n2 
               mbool = Gate NAND n1 n1 in
           ifThenElse ( n1 && nbool) n1 (ifThenElse (mbool && n2) n1 (imp n1s n2s))
    imp _ _ = error "Bad args for imp"

numEq :: SecureFunction a
numEq n1 n2 = [foldl1 (&&) (zipWith bij n1 n2)]

hammingDist :: SecureFunction a
hammingDist n1 n2 = let (len,total) = hammingDistF n1 n2 in total

hammingDistF [n1] [n2] = (1,lbij n1 n2)
hammingDistF (n1:n1s) (n2:n2s) = let (len,subtotal) = hammingDistF n1s n2s in
                                     let (leng,(carry,remsum)) = addIntF len subtotal 1 (lbij n1 n2) in
                                         (leng,(carry:[])++remsum)

--hammingDist n1 n2 = [foldl1 addInt (zipWith lbij  n1 n2)]



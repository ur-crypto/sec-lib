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
testb64 = 744073709551616 

test64 :: Int64
test64 = 395648674974903

testb8 :: Int8
testb8 = 12



addIntFP :: Int -> Int -> [Node a1] -> Int -> [Node a1] -> (Int, (Node a1, [Node a1]))
addIntFP p m1 [n1] m2 [n2] = (1,(n1 && n2,((Gate XOR n1 n2):[])))
addIntFP p m1 (n1:n1s) m2 (n2:n2s) = 
  let cond1 = (m1 > p) 
      cond2 = (m2 > p)
      cond3 = (m1 > m2)
      cond4 = (m1 < m2) in
      case (cond1,cond2,cond3,cond4) of
          (True,True,_,_)             -> addIntFP p (m1-1) n1s (m2-1) n2s 
          (True,False,_,_)            -> addIntFP p (m1-1) n1s m2 (n2:n2s) 
          (False,True,_,_)            -> addIntFP p m1 (n1:n1s) (m2-1) n2s 
          (False,False,False,False)   -> let generate = (n1:n1s) .&. (n2:n2s)
                                             propogate = xor (n1:n1s) (n2:n2s) in
                                             subCompute propogate generate  
          (False,False,True,_)        -> let (len,(carry,remsum)) = addIntFP p (m1-1) n1s m2 (n2:n2s) in
                                         (len+1,((carry && n1),((Gate XOR n1 carry):[])++remsum))
          (False,False,False,True)    -> let (len,(carry,remsum)) = addIntFP p m1 (n1:n1s) (m2-1) n2s in
                                         (len+1,((carry && n2),((Gate XOR n2 carry):[])++remsum)) 

subCompute [p1] [g1]  = (1,(g1,p1:[]))
subCompute (p1:p1s) (g1:g1s) =
      let (len,(carry,prevcarry)) = subCompute p1s g1s in
          (len+1,((Gate XOR g1 (carry && p1)),(((Gate XOR p1 carry):[])++prevcarry)))
          


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
hammingDist n1 n2 = let difference = xor n1 n2 in
                        let (len,distance) = hammingWeight 64 difference in
                            distance


hammingWeight :: Int -> [Node a1] -> (Int, [Node a1])
hammingWeight p n = 
    case (p>1) of
         (True)     ->    let (leftHalf,rightHalf) = splitAt (quot p 2) n in
                               let (lenleft,subleft) = hammingWeight (quot p 2) leftHalf
                                   (lenright,subright) = hammingWeight (quot p 2) rightHalf in
                                    let (len,(carry,subTotal)) = addIntFP 8 lenleft subleft lenright subright in
                                          (len+1,((carry:[])++subTotal))
         (False)    ->    (1,n)

--(len,subtotal) = hammingDistF n1s n2s in
--                                   let (leng,(carry,remsum)) = addIntFP 5 len subtotal 1 (lbij n1 n2) in
--                                         (leng+1,(carry:[])++remsum)

--hammingDist n1 n2 = [foldl1 addInt (zipWith lbij  n1 n2)]



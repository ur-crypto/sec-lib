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


andShift :: SecureFunction a
andShift xs ys = (shiftL 1 xs) .&. ys

addInt m n = let (len,(carry,subTotal)) =  addIntFP 32 32 m 32 n in 
                 subTotal

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
          (False,False,True,_)        -> let (len,(carry,resltsum)) = addIntFP p (m1-1) n1s m2 (n2:n2s) in
                                         (len+1,((carry && n1),((Gate XOR n1 carry):[])++resltsum))
          (False,False,False,True)    -> let (len,(carry,resltsum)) = addIntFP p m1 (n1:n1s) (m2-1) n2s in
                                         (len+1,((carry && n2),((Gate XOR n2 carry):[])++resltsum)) 

subCompute [p1] [g1]  = (1,(g1,p1:[]))
subCompute (p1:p1s) (g1:g1s) =
      let (len,(carry,prevcarry)) = subCompute p1s g1s in
          (len+1,((Gate XOR g1 (carry && p1)),(((Gate XOR p1 carry):[])++prevcarry)))
          


numCmp :: SecureFunction a
numCmp as bs = [imp as bs]
    where
    imp :: [Node a] -> [Node a] -> Node a
    imp [] [] = Constant True 
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
                        let (len,distance) = hammingWeight 64  difference in
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

ueand (n1:n1s) [] = let reslt = ueand n1s [] in
                              (((Constant False):[])++reslt)
ueand [] (n1:n1s) = let reslt = ueand n1s [] in
                              (((Constant False):[])++reslt) 
ueand [] [n1] = (Constant False):[]
ueand [n1] [] = (Constant False):[]
ueand [] [] = []
ueand [n1] [n2] = (n1 && n2):[]
ueand (n1:n1s) (n2:n2s) = let reslt = ueand n1s n2s in
                              (((n1 && n2):[])++reslt)

ureand n1 n2 = reverse (ueand (reverse n1) (reverse n2))
urexor n1 n2 = reverse (uexor (reverse n1) (reverse n2))

uexor (n1:n1s) [] = (n1:n1s)
uexor [] (n1:n1s) = (n1:n1s) 
uexor [] [n1] = n1:[]
uexor [n1] [] = n1:[]
uexor [n1] [n2] = (b_xor n1 n2):[]
uexor (n1:n1s) (n2:n2s) = let reslt = uexor n1s n2s in
                              (((b_xor n1 n2):[])++reslt)

hammingWt :: Int -> [Node a1] -> (Int, [Node a1])
hammingWt p n =
    case (p>2,p>1) of
         (True,_)        -> let (leftThird,rightHalf) = splitAt (quot p 3) n in
                             let (midThird,rightThird) = splitAt (quot p 3) rightHalf in
                                  let (lenleft,subleft) = hammingWt (quot p 3) leftThird
                                      (lenmid,submid) = hammingWt (quot p 3) midThird
                                      (lenright,subright) = hammingWt (p-2*(quot p 3)) rightThird in
                                      let fsummand = (urexor) ((urexor) subleft submid) subright 
                                          ssummand = ((urexor) ((urexor) (ureand subleft submid) (ureand submid  subright)) (ureand subleft subright))++((Constant False):[])
                                          mx = maximum((lenleft:lenright:lenmid:[])) in
                                          let (len,(carry,subTotal)) = addIntFP 8 mx fsummand (mx+1) ssummand in
                                              (len+1,((carry:[])++subTotal))
         (False,True)    -> let (fb:[sb]) = n in (2,((fb && sb):((b_xor) fb sb):[])) 
         (False,False)   -> (1,n)




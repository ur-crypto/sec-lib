module Examples where
import Types
import Utils
import Ops as O
import Data.Int
import Data.Bits
import Data.Array
import Prelude hiding ((&&), (||), ifThenElse,ifThenElses)


test8 :: Int8
test8 = 35

testb8 :: Int8
testb8 = 74


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


--andShift :: SecureFunction a
--andShift xs ys = (O.shiftL 1 xs) O..&. ys

addInt :: [Node a1] -> [Node a1] -> [Node a1]
addInt m n = let lenm = length m
                 lenn = length n in
                       let (len,(carry,subTotal)) =  addIntFP 16 lenm m lenn n in 
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
-- addIntFP _ _ _ _ _ = error "unbalanced inputs"

subCompute :: [Node a] -> [Node a] -> (Int, (Node a, [Node a]))
subCompute [p1] [g1]  = (1,(g1,p1:[]))
subCompute (p1:p1s) (g1:g1s) =
      let (len,(carry,prevcarry)) = subCompute p1s g1s in
          (len+1,((Gate XOR g1 (carry && p1)),(((Gate XOR p1 carry):[])++prevcarry)))
-- subCompute _ _ = error "unbalanced inputs"

numCmps :: [Node a] -> [Node a] -> Node a
numCmps as bs = Constant False
{-numCmps as bs = imp as bs
    where
    imp :: [Node a] -> [Node a] -> Node a
    imp [n1] [n2] = 
           let nbool = Not n2 in
           n1 && nbool
    imp (n1:n1s) (n2:n2s) =
           let nbool = Not n2 
               mbool = Not n1 
               l1 = length n1s
               l2 = length n2s in
               case (l1 > l2,l1 < l2) of
                    (True,_)      -> n1 || (imp n1s (n2:n2s))
                    (False,False) -> ifThenElse ( n1 && nbool) n1 (ifThenElse (mbool && n2) n1 (imp n1s n2s))
                    (False,True)  -> n2 || (imp (n1:n1s) n2s)
    imp _ _ = error "Bad args for imp"
-}

numCmp :: SecureFunction a
numCmp as bs = [imp as bs]
    where
    imp :: [Node a] -> [Node a] -> Node a
    imp [n1] [n2] = 
           let nbool = Not n2 in
           n1 && nbool
    imp (n1:n1s) (n2:n2s) =
           let nbool = Not n2 
               mbool = Not n1 in
           ifThenElse ( n1 && nbool) n1 (ifThenElse (mbool && n2) n1 (imp n1s n2s))
    imp _ _ = error "Bad args for imp"

numEqs :: [Node a] -> [Node a] -> Node a          
numEqs n1 n2 = foldl1 (&&) (zipWith bij n1 n2)

numEq :: SecureFunction a
numEq n1 n2 = [foldl1 (&&) (zipWith bij n1 n2)]

hammingDist :: SecureFunction a
hammingDist n1 n2 = let difference = xor n1 n2 in
                        let (len,distance) = hammingWeight 64  difference in
                            (Constant False:[])++distance


hammingWeight :: Int -> [Node a1] -> (Int, [Node a1])
hammingWeight p n = 
    case (p>1) of
         (True)     ->    let (leftHalf,rightHalf) = splitAt (quot p 2) n in
                               let (lenleft,subleft) = hammingWeight (quot p 2) leftHalf
                                   (lenright,subright) = hammingWeight (quot p 2) rightHalf in
                                    let (len,(carry,subTotal)) = addIntFP 6 lenleft subleft lenright subright in
                                          (len+1,((carry:[])++subTotal))
         (False)    ->    (1,n)

ourBool (n1:n1s) = case n1 of
                       (True)   ->    ((Constant True):[]) ++ (ourBool n1s)
                       (False)  ->    ((Constant False):[]) ++ (ourBool n1s)
ourBool [] = []

{-
editDistance :: SecureFunction a 
editDistance xs ys = table ! (m,n)
    where
    m     = 8 :: Int
    n     = 8 :: Int
    --(m,n) = (length xs, length ys)
    x     = array (1,m) (zip [1..] xs)
    y     = array (1,n) (zip [1..] ys)
 
    --table :: Array (Int,Int) [Node a]
    table = array bnds [(ij, dist ij) | ij <- range bnds]
    bnds  = ((0,0),(m,n))
    --bnds  = ((0,0),(m,n))
    dist (0,0) = [Constant False] 
    --dist (0,j) = let (l1,(b1,a1)) = addIntFP 4 (length (table ! (0,j-1))) (table ! (0,j-1)) 1 ((Constant True):[]) in (b1:[])++a1
    --dist (i,0) = let (l1,(b1,a1)) = addIntFP 4 (length (table ! (i-1,0))) (table ! (i-1,0)) 1 ((Constant True):[]) in (b1:[])++a1
    dist (0,j) = ourBool (bitsToBools (fromIntegral (j :: Int) :: Int8))
    dist (i,0) = ourBool (bitsToBools (fromIntegral (i :: Int) :: Int8))
    dist (i,j) = let (li,(carry,intermed)) = addIntFP 4 (length (table ! (i-1,j-1))) (table ! (i-1,j-1)) 1 ((Constant True):[]) in 
                     let result = ifThenElse (numCmps (table ! (i-1,j)) (table ! (i,j-1))) (table ! (i,j-1))
                                          (ifThenElse (numCmps (table ! (i-1,j-1)) ((carry:[])++intermed)) ((carry:[])++intermed) (table ! (i-1,j))) in
                              let (l1,(b1,a1)) = addIntFP 4 (length result) result 1 ((Constant True):[]) in
                              (b1:[])++a1
                     -- ifThenElses (numCmps ((b1:[])++a1) ((b2:[])++a2)) ((b2:[])++a2) ((b1:[])++a1)
    --dist (i,j) = a1 --ifThenElses (numCmps a1 a2) a2 (ifThenElses (numCmps a1 a3) a3 a1)
                    -- a3 = ifThenElses (Gate XOR (x ! i) (y ! i)) ab3 aa3
                      --   where
                        -- aa3 = table ! (i-1,j-1)
                        -- (la3,(bb3,ab3)) = addIntFP 8 (length (table ! (i-1,j-1))) (table ! (i-1,j-1)) 2 ((Constant True):(Constant False):[]) 

-- minimum [table ! (i-1,j) + 1, table ! (i,j-1) + 1,
--        if x ! i == y ! j then table ! (i-1,j-1) else 1 + table ! (i-1,j-1)]

-}

ueand :: [Node a] -> [Node a] -> [Node a]
ueand (n1:n1s) [] = let reslt = ueand n1s [] in
                              (((Constant False):[])++reslt)
ueand [] (n1:n1s) = let reslt = ueand n1s [] in
                              (((Constant False):[])++reslt) 
--ueand [] [n1] = (Constant False):[]
--ueand [n1] [] = (Constant False):[]
ueand [] [] = []
ueand [n1] [n2] = (n1 && n2):[]
ueand (n1:n1s) (n2:n2s) = let reslt = ueand n1s n2s in
                              (((n1 && n2):[])++reslt)

urexor :: [Node a] -> [Node a] -> [Node a]
ureand :: [Node a] -> [Node a] -> [Node a]
ureand n1 n2 = reverse (ueand (reverse n1) (reverse n2))
urexor n1 n2 = reverse (uexor (reverse n1) (reverse n2))

uexor :: [Node a] -> [Node a] -> [Node a]
uexor (n1:n1s) [] = (n1:n1s)
uexor [] (n1:n1s) = (n1:n1s) 
--uexor [] [n1] = n1:[]
--uexor [n1] [] = n1:[]
uexor [] [] = []
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
                                          let (len,(carry,subTotal)) = addIntFP 6 mx fsummand (mx+1) ssummand in
                                              (len+1,((carry:[])++subTotal))
         (False,True)    -> let (fb:[sb]) = n in (2,((fb && sb):((b_xor) fb sb):[])) 
         (False,False)   -> (1,n)

{-
levenshtein2 :: SecureFunction a
levenshtein2 sa sb = last $ foldl transform [0..length sa] sb 
   where
         transform xs@(x:xs') c = scanl compute (x+1) (zip3 sa xs xs') 
                  where
                              compute z (c', x, y) = minimum [y+1, z+1, x + fromEnum (c' /= c)]
                              -}

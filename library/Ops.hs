{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Ops where
import Types
import Utils
import Data.Bits
import Prelude hiding ((&&), (||), not)

--Gate Macros

(&&) :: Node a -> Node a -> Node a
(&&) = processConstant AND
(||) :: Node a -> Node a -> Node a
(||) = processConstant OR
b_xor :: Node a -> Node a -> Node a
b_xor = processConstant XOR
nand :: Node a -> Node a -> Node a
nand = processConstant NAND
bij :: Node a -> Node a -> Node a
bij = processConstant BIJ
not :: Node a -> Node a
not = Not

--Bit macros
(.~&.) :: SecureFunction a
(.~&.) = zipWith nand

(<.) :: SecureFunction a
(<.) as bs = [imp as bs]
    where
    imp :: [Node a] -> [Node a] -> Node a
    imp [n1] [n2] = 
           let nbool = not n2 in
           n1 && nbool
    imp (n1:n1s) (n2:n2s) =
           let nbool = not n2 
               mbool = not n1 in
           ifThenElse ( n1 && nbool) n1 (ifThenElse (mbool && n2) n1 (imp n1s n2s))
    imp _ _ = error "Bad args for imp"

(==.) :: SecureFunction a
(==.) n1 n2 = [foldl1 (&&) (zipWith bij n1 n2)]
(/=.) :: SecureFunction a
(/=.) a b = complement (a ==. b)

--If Then Else Macro
ifThenElse :: Node a -> Node a -> Node a -> Node a
ifThenElse bool tb fb = 
    let nbool = Gate NAND bool bool in
    ((bool && tb) || (nbool && fb))

if' :: SecureNum a -> SecureNum a -> SecureNum a -> SecureNum a
if' bools= zipWith (ifThenElse (foldl1 (||) bools))

num2Const :: Int -> SecureNum a
num2Const n = map Constant (bits2Bools n)

extendBy :: Int -> SecureNum a -> SecureNum a
extendBy n x = (map (\_->Constant False) [0..n-1]) ++ x


--add def
addInt :: [Node a1] -> [Node a1] -> [Node a1]
addInt m n = let lenm = length m
                 lenn = length n in
                       let (_,(_,subTotal)) =  addIntFP (length m) lenm m lenn n in
                           subTotal

addIntFP :: Int -> Int -> [Node a1] -> Int -> [Node a1] -> (Int, (Node a1, [Node a1]))
addIntFP _ _ [n1] _ [n2] = (1,(n1 && n2,((Gate XOR n1 n2):[])))
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
addIntFP _ _ _ _ _ = error "unbalanced inputs"

subCompute :: [Node a] -> [Node a] -> (Int, (Node a, [Node a]))
subCompute [p1] [g1]  = (1,(g1,p1:[]))
subCompute (p1:p1s) (g1:g1s) =
      let (len,(carry,prevcarry)) = subCompute p1s g1s in
          (len+1,((Gate XOR g1 (carry && p1)),(((Gate XOR p1 carry):[])++prevcarry)))
subCompute _ _ = error "unbalanced inputs"

instance Num (SecureNum a) where
    (+) = addInt

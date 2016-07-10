{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Ops where
import           Data.Bits
import           Gate
import           Prelude                    hiding (not, (&&), (||))
import           Types
import           Utils

import           Control.Monad.State.Strict

import           Data.Graph.Inductive.Graph

--Gate Macros

(&&) :: SecureGate
(&&) = andGate
(||) :: SecureGate
(||) = orGate
bXor :: SecureGate
bXor = xorGate
nand :: SecureGate
nand a b = notGate $ andGate a b
bij :: SecureGate
bij a b = notGate $ xorGate a b
not :: GraphBuilder -> GraphBuilder
not = notGate

--Bit macros
(.~&.) :: SecureFunction
(.~&.) = zipWith nand

(<.) :: SecureFunction
(<.) as bs = [imp as bs]
    where
    imp :: SecureNum -> SecureNum -> GraphBuilder
    imp [n1] [n2] =
           let nbool = not n2 in
           n1 && nbool
    imp (n1:n1s) (n2:n2s) =
           let nbool = not n2
               mbool = not n1 in
           ifThenElse ( n1 && nbool) n1 (ifThenElse (mbool && n2) n1 (imp n1s n2s))
    imp _ _ = error "Bad args for imp"

(==.) :: SecureFunction
(==.) n1 n2 = [foldl1 (&&) (zipWith bij n1 n2)]
(/=.) :: SecureFunction
(/=.) a b = complement (a ==. b)

--If Then Else Macro
ifThenElse :: GraphBuilder -> GraphBuilder -> GraphBuilder -> GraphBuilder
ifThenElse bool tb fb =
    let nbool = not bool in
    (bXor (bool && tb) (nbool && fb))

if' :: SecureNum -> SecureNum -> SecureNum -> SecureNum
if' bools= zipWith (ifThenElse (foldl1 (||) bools))


genConstant :: (MonadState Node m, DynGraph gr) => Bool -> m (gr SecureNode b)
genConstant bool = do
  number <- get
  return $ insNode (number, Constant bool) empty

num2Const :: FiniteBits a => a -> SecureNum
num2Const n =
  map genConstant (bits2Bools n)

extendBy :: Int -> SecureNum -> SecureNum
extendBy n x = (map (const $ genConstant False) [0..n-1]) ++ x


--add def
addInt :: [GraphBuilder] -> [GraphBuilder] -> [GraphBuilder]
addInt m n = let (_,(_,subTotal)) =  addIntFP (length m) m n in
                           subTotal

addIntFP :: Int -> [GraphBuilder] -> [GraphBuilder] -> (Int, (GraphBuilder, [GraphBuilder]))
addIntFP _ [n1] [n2] = (1,(n1 && n2,((bXor n1 n2):[])))
addIntFP p (n1:n1s) (n2:n2s) =
  let m1 = 1+(length n1s)
      m2 = 1+(length n2s) in
    let cond1 = (m1 > p)
        cond2 = (m2 > p)
        cond3 = (m1 > m2)
        cond4 = (m1 < m2) in
         case (cond1,cond2,cond3,cond4) of
          (True,True,_,_)             -> addIntFP p n1s n2s
          (True,False,_,_)            -> addIntFP p n1s (n2:n2s)
          (False,True,_,_)            -> addIntFP p (n1:n1s) n2s
          (False,False,False,False)   -> let generate = (n1:n1s) .&. (n2:n2s)
                                             propogate = xor (n1:n1s) (n2:n2s) in
                                             subCompute propogate generate
          (False,False,True,_)        -> let (len,(carry,resltsum)) = addIntFP p n1s (n2:n2s) in
                                         (len+1,((carry && n1),((bXor n1 carry):[])++resltsum))
          (False,False,False,True)    -> let (len,(carry,resltsum)) = addIntFP p (n1:n1s) n2s in
                                         (len+1,((carry && n2),((bXor n2 carry):[])++resltsum))
addIntFP _ _ _ = error "unbalanced inputs"

subCompute :: [GraphBuilder] -> [GraphBuilder] -> (Int, (GraphBuilder, [GraphBuilder]))
subCompute [p1] [g1]  = (1,(g1,p1:[]))
subCompute (p1:p1s) (g1:g1s) =
      let (len,(carry,prevcarry)) = subCompute p1s g1s in
          (len+1,((bXor g1 (carry && p1)),(((bXor p1 carry):[])++prevcarry)))
subCompute _ _ = error "unbalanced inputs"

instance Num (SecureNum) where
    (+) = addInt

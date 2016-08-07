{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Ops where
import           Data.Bits
import Debug.Trace
import           Data.Int
import           Data.List

import           Prelude                    hiding (not, (&&), (||))
import           Types
import           Utils

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
not :: SecureGraphBuilder -> SecureGraphBuilder
not = notGate
--Bit macros
(.~&.) :: SecureFunction
(.~&.) = zipWith nand

(<.) :: SecureFunction
(<.) as bs = [imp as bs]
    where
    imp :: SecureNum -> SecureNum -> SecureGraphBuilder
    imp [n1] [n2] =
      trace "<." $
           n1 && not n2
    imp (n1:n1s) (n2:n2s) =
           ifThenElse ( n1 && not n2) n1 (ifThenElse (not n1 && n2) n1 (imp n1s n2s))
    imp n m = error $ "Bad args for imp" ++ show (length n) ++ show (length m)

(==.) :: SecureFunction
(==.) n1 n2 = [foldl1 (&&) (zipWith bij n1 n2)]
(/=.) :: SecureFunction
(/=.) a b = complement (a ==. b)

--If Then Else Macro
ifThenElse :: SecureGraphBuilder -> SecureGraphBuilder -> SecureGraphBuilder -> SecureGraphBuilder
ifThenElse bool tb fb =
    let nbool = not bool in
    (bXor (bool && tb) (nbool && fb))

if' :: SecureNum -> SecureNum -> SecureNum -> SecureNum
if' bools= trace ("if") $ zipWith (ifThenElse (foldl1 (||) bools))

num2Const :: FiniteBits a => a -> SecureNum
num2Const n =
  map wrapConstant (bits2Bools n)

extendBy :: Int -> SecureNum -> SecureNum
extendBy n x = (map (const $ wrapConstant False) [0..n-1]) ++ x


--add def
addInt :: [SecureGraphBuilder] -> [SecureGraphBuilder] -> [SecureGraphBuilder]
addInt m n = let (_,(_,subTotal)) =  addIntFP (length m) m n in
                           subTotal

addIntFP :: Int -> [SecureGraphBuilder] -> [SecureGraphBuilder] -> (Int, (SecureGraphBuilder, [SecureGraphBuilder]))
addIntFP _ [n1] [n2] = (1,(n1 && n2,((bXor n1 n2):[])))
addIntFP p (n1:n1s) (n2:n2s) =
  trace "add" $
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

subCompute :: [SecureGraphBuilder] -> [SecureGraphBuilder] -> (Int, (SecureGraphBuilder, [SecureGraphBuilder]))
subCompute [p1] [g1]  = (1,(g1,p1:[]))
subCompute (p1:p1s) (g1:g1s) =
      let (len,(carry,prevcarry)) = subCompute p1s g1s in
          (len+1,((bXor g1 (carry && p1)),(((bXor p1 carry):[])++prevcarry)))
subCompute _ _ = error "unbalanced inputs"

instance Num (SecureNum) where
    (+) = addInt
    (*) = undefined
    signum = undefined
    fromInteger = undefined
    negate = undefined

levenshtein2 :: SecureFunction
levenshtein2 sa sb = last $ foldl' transform (map num2Const [0..fromIntegral (length sa) :: Int8] ) sb
    where
        transform xs@(x:xs') c = scanl' compute (x+(num2Const (1 :: Int8))) (zip3 sa xs xs')
            where
                compute z (c', x', y) = foldl1' cmp [y+(num2Const (1 :: Int8)), z+(num2Const (1 :: Int8)), x' + [bXor c' c]]
                    where
                        cmp a b = if' (a <. b) a b
levenshteinBad :: SecureFunction
levenshteinBad sa sb = last $ foldl' transform (map num2Const [0..fromIntegral (length sa) :: Int8] ) sb
    where
        transform xs@(x:xs') c = scanl' compute (x+(num2Const (1 :: Int8))) (zip3 sa xs xs')
            where
                compute z (c', x', y) = foldl1' cmp [y+(num2Const (1 :: Int8)), z+(num2Const (1 :: Int8)), x' + [bXor c' c]]
                    where
                        cmp a b = if' a a b

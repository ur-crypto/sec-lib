module Ops where
import Types
import qualified Prelude as P

--Gate Macros
(&&) :: Node a -> Node a -> Node a
(&&) = Gate AND
(||) :: Node a -> Node a -> Node a
(||) = Gate OR
b_xor :: Node a -> Node a -> Node a
b_xor = Gate XOR
nand :: Node a -> Node a -> Node a
nand = Gate NAND
bij :: Node a -> Node a -> Node a
bij = Gate BIJ

--Bit macros
(.&.) :: SecureFunction a
(.&.) xs ys = P.zipWith (&&) xs ys
(.|.) :: SecureFunction a
(.|.) xs ys = P.zipWith (||) xs ys
xor :: SecureFunction a
xor xs ys = P.zipWith b_xor xs ys
(.~&.) :: SecureFunction a
(.~&.) xs ys = P.zipWith (nand) xs ys
complement :: [Node a] -> [Node a]
complement xs = xor xs xs

(<.) :: SecureFunction a
(<.) as bs = [imp as bs]
    where
    imp :: [Node a] -> [Node a] -> Node a
    imp [n1] [n2] = 
           let nbool = Gate NAND n2 n2 in
           n1 && nbool
    imp (n1:n1s) (n2:n2s) =
           let nbool = Gate NAND n2 n2 
               mbool = Gate NAND n1 n1 in
           ifThenElse ( n1 && nbool) n1 (ifThenElse (mbool && n2) n1 (imp n1s n2s))
    imp _ _ = P.error "Bad args for imp"

(==.) :: SecureFunction a
(==.) n1 n2 = [P.foldl1 (&&) (P.zipWith bij n1 n2)]

--Bit Const macros
shiftL :: P.Int -> [Node a] -> [Node a]
shiftL num xs = (P.drop num xs) P.++ P.map (P.const P.$ Constant P.False) [0 .. num]
rotate :: P.Int -> [Node a] -> [Node a]
rotate _ [] = []
rotate n xs = P.zipWith P.const (P.drop n (P.cycle xs)) xs

--If Then Else Macro
ifThenElse :: Node a -> Node a -> Node a -> Node a
ifThenElse bool tb fb = 
    let nbool = Gate NAND bool bool in
    ((bool && tb) || (nbool && fb))


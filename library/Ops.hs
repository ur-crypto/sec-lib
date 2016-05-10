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
not :: Node a -> Node a
not = Not

--Bit macros
(.&.) :: SecureFunction a
(.&.) = P.zipWith (&&)
(.|.) :: SecureFunction a
(.|.) = P.zipWith (||)
xor :: SecureFunction a
xor = P.zipWith b_xor
(.~&.) :: SecureFunction a
(.~&.) = P.zipWith nand
complement :: [Node a] -> [Node a]
complement = P.map not

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
    imp _ _ = P.error "Bad args for imp"

(==.) :: SecureFunction a
(==.) n1 n2 = [P.foldl1 (&&) (P.zipWith bij n1 n2)]

--Bit Const macros
shiftL :: [Node a] -> P.Int -> [Node a]
shiftL xs num = (P.drop num xs) P.++ P.map (P.const P.$ Constant P.False) [0 .. num P.-1]
rotate :: [Node a] -> P.Int -> [Node a]
rotate xs n = P.zipWith P.const (P.drop n (P.cycle xs)) xs

--If Then Else Macro
ifThenElse :: Node a -> Node a -> Node a -> Node a
ifThenElse bool tb fb = 
    let nbool = Gate NAND bool bool in
    ((bool && tb) || (nbool && fb))


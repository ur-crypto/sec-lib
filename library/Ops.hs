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
complement :: [Node a] -> [Node a]
complement xs = xor xs xs

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


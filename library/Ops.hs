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

--If Then Else Macro
ifThenElse :: Node a -> Node a -> Node a -> Node a
ifThenElse bool tb fb = 
    let nbool = Gate NAND bool bool in
    ((bool && tb) || (nbool && fb))


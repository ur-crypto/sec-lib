module Types (
                Value(..)
              , GateType(..)
              , Key
              , TruthTable(..)
              , (.&.)
              , (.|.)
              , xor
              , nand
              , bij
              , ifThenElse
              , TestBool
              )
              where
import Data.ByteString

type Key = ByteString

type TestBool a = [Value a] -> [Value a] -> Value a

data GateType   = AND
                | OR
                | XOR
                | NAND
                | BIJ deriving(Show)
data Value a    = Gate GateType (Value a) (Value a)
                | Input a deriving(Show)
data TruthTable a = TruthTable a a a a

--Circuit Macros
(.&.) :: Value a -> Value a -> Value a
(.&.) = Gate AND
(.|.) :: Value a -> Value a -> Value a
(.|.) = Gate OR
xor :: Value a -> Value a -> Value a
xor = Gate XOR 
nand :: Value a -> Value a -> Value a
nand = Gate NAND
bij :: Value a -> Value a -> Value a
bij = Gate BIJ

--If Then Else Macro
ifThenElse :: Value a -> Value a -> Value a -> Value a
ifThenElse bool tb fb = 
    let nbool = Gate NAND bool bool in
    ((bool .&. tb) .|. (nbool .&. fb))


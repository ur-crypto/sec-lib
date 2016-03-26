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
              )
              where
import Data.ByteString

type Key = ByteString

data GateType   = AND
                | OR
                | XOR
                | NAND
                | BIJ
data Value a    = Gate GateType (Value a) (Value a)
                | Input a
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


module Types (Value(..), GateType(..))  where
data GateType   = AND
                | OR
                | XOR
                | NAND

data Value a    = Gate GateType (Value a) (Value a)
                | Input a


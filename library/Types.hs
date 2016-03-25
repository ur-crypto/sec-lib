module Types (Value(..), GateType(..), Key)  where
import Data.ByteString

type Key = ByteString


data GateType   = AND
                | OR
                | XOR
                | NAND

data Value a    = Gate GateType (Value a) (Value a)
                | Input a

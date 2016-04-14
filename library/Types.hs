module Types (
                Node(..)
              , GateType(..)
              , Key
              , TruthTable(..)
              , SecureFunction
              )
              where

import Data.ByteString as BS
import qualified Prelude as P

type Key = BS.ByteString

type SecureFunction a = [Node a] -> [Node a] -> [Node a]

data GateType   = AND
                | OR
                | XOR
                | NAND
                | BIJ deriving(P.Show)

data Node a    = Gate GateType (Node a) (Node a)
                | Not (Node a)
                | Constant P.Bool
                | Input a deriving(P.Show)

data TruthTable a = TruthTable a a a a


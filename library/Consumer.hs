module Consumer where
import Data.Bits
import Data.Word

data Input a = IO a
data Key = Input Word64 deriving(Show)
data KeyPair = KeyPair{key1::Key, key2::Key} deriving(Show)
data TruthTable = TruthTable{
      row1 :: (Key, Key, Key)
    , row2 :: (Key, Key, Key) 
    , row3 :: (Key, Key, Key)
    , row4 :: (Key, Key, Key)
    }
    deriving(Show)


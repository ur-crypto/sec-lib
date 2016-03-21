module Producer where
import Data.Bits
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Utils

type Key = (IO B.ByteString)
data KeyPair = KeyPair{key1::Key, key2::Key}
data TruthTable = TruthTable{
      row1 :: (Key, Key, Key)
    , row2 :: (Key, Key, Key) 
    , row3 :: (Key, Key, Key)
    , row4 :: (Key, Key, Key)
    }

printTT :: TruthTable -> IO()
printTT (TruthTable row1 row2 row3 row4 )= do
    printRow row1
    printRow row2
    printRow row3
    printRow row4
    return ()
    where
    printRow :: (Key, Key, Key) -> IO()
    printRow (io1, io2, io3) = do
        k1 <- io1
        k2 <- io2
        k3 <- io3
        print $ show (B8.unpack k1) ++ " " ++ show (B8.unpack k2) ++ " " ++ show (B8.unpack k3)
        return ()


(.&.) (a0, a1) (b0, b1) = do
    o0 <- getKey
    o1 <- getKey
    let oi0 = return o0 :: (IO B.ByteString)
    let oi1 = return o1 :: (IO B.ByteString)
    printTT $ TruthTable (a0, b0, oi0) (a1, b0, oi0) (a0, b1, oi0) (a1, b1, oi1)
    return (o0, o1)


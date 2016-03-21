module Producer where
import Data.Bits
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Utils

type Key = B.ByteString
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
    printRow (k1, k2, k3) = do
        print $ show (B8.unpack k1) ++ " " ++ show (B8.unpack k2) ++ " " ++ show (B8.unpack k3)
        return ()


(.&.) :: IO (Key, Key) -> IO(Key, Key) -> IO (Key, Key)
(.&.) kp1 kp2 = do
    (a0, a1) <- kp1
    (b0, b1) <- kp2
    o0 <- getKey
    o1 <- getKey
    printTT $ TruthTable (a0, b0, o0) (a1, b0, o0) (a0, b1, o0) (a1, b1, o1)
    return (o0, o1)

(.|.) :: IO (Key, Key) -> IO(Key, Key) -> IO (Key, Key)
(.|.) kp1 kp2 = do
    (a0, a1) <- kp1
    (b0, b1) <- kp2
    o0 <- getKey
    o1 <- getKey
    printTT $ TruthTable (a0, b0, o0) (a1, b0, o1) (a0, b1, o1) (a1, b1, o1)
    return (o0, o1)

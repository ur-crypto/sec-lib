module Producer where
import qualified Data.ByteString as B
import Data.ByteString.Internal (unpackBytes)
import qualified Codec.Binary.BubbleBabble as X
import Utils

type Key = B.ByteString
data KeyPair = KeyPair{key1::Key, key2::Key}
data TruthTable = TruthTable (Key, Key, Key) (Key, Key, Key) (Key, Key, Key) (Key, Key, Key)

printTT :: TruthTable -> IO()
printTT (TruthTable row1 row2 row3 row4 )= do
    putStrLn "Truth Table As Follows"
    printRow row1
    printRow row2
    printRow row3
    printRow row4
    return ()
    where
    printRow :: (Key, Key, Key) -> IO()
    printRow (k1, k2, k3) = do
        let s_list = map (X.encode . unpackBytes) [k1, k2, k3]
        putStrLn $ unlines s_list
        return ()

unwrapKeys :: IO(Key, Key) -> IO(Key, Key) -> IO(Key, Key, Key, Key)
unwrapKeys kp1 kp2 = do
    (a0, a1) <- kp1
    (b0, b1) <- kp2
    return (a0, a1, b0, b1)

printSendTT :: TruthTable -> IO()
printSendTT (TruthTable r1 r2 r3 r4) =  do
    let outkeys = map encOutKey [r1, r2, r3, r4]
    -- network send
    putStrLn "Encrypted Keys"
    putStrLn $ unlines $ map (X.encode . unpackBytes) outkeys

getKeys :: IO(Key, Key) -> IO(Key, Key) -> IO(Key, Key, Key, Key, Key, Key)
getKeys kp1 kp2 = do
    (a0, a1, b0, b1) <- unwrapKeys kp1 kp2 
    (o0, o1) <- genKeyPair
    return (a0, a1, b0, b1, o0, o1)

sendInfo :: TruthTable -> IO()
sendInfo tt = do
    printTT tt
    printSendTT tt

-- define these as a type
(.&.) :: IO (Key, Key) -> IO(Key, Key) -> IO (Key, Key)
(.&.) kp1 kp2 = do
    (a0, a1, b0, b1, o0, o1) <- getKeys kp1 kp2
    sendInfo $ TruthTable (a0, b0, o0) (a1, b0, o0) (a0, b1, o0) (a1, b1, o1)
    return (o0, o1)

(.|.) :: IO (Key, Key) -> IO(Key, Key) -> IO (Key, Key)
(.|.) kp1 kp2 = do
    (a0, a1, b0, b1, o0, o1) <- getKeys kp1 kp2
    sendInfo $ TruthTable (a0, b0, o0) (a1, b0, o1) (a0, b1, o1) (a1, b1, o1)
    return (o0, o1)

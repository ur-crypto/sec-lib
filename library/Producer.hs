{-# LANGUAGE RebindableSyntax #-}
module Producer where
import Prelude hiding (ifThenElse)
import qualified Data.ByteString as B
import Data.ByteString.Internal (unpackBytes)
import qualified Codec.Binary.BubbleBabble as X
import Utils
import Types

type Key = B.ByteString
type KeyPair = Value (Key, Key)
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

printSendTT :: TruthTable -> IO()
printSendTT (TruthTable r1 r2 r3 r4) =  do
    let outkeys = map encOutKey [r1, r2, r3, r4]
    -- network send
    putStrLn "Encrypted Keys"
    putStrLn $ unlines $ map (X.encode . unpackBytes) outkeys
    putStrLn "Testing Dec"
    testDec r1 r2 r3 r4 outkeys
    where 
    testDec (a0, b0, _) (a1, b1, _) (a2, b2, _) (a3, b3, _) outkeys = do
        putStrLn "Should Be Correct"
        printInfo [(a0, b0, outkeys !! 0), (a1, b1, outkeys !! 1), (a2, b2, outkeys !! 2), (a3, b3, outkeys !! 3)]
        putStrLn "Should Be Incorrect"
        printInfo [(a0, b0, outkeys !! 1), (a1, b1, outkeys !! 2), (a2, b2, outkeys !! 3), (a3, b3, outkeys !! 0)]
        where
        printInfo x = putStrLn $ unlines (map (process . decOutKey) x)
        process (Just a) = X.encode . unpackBytes $ a
        process Nothing = ""

sendInfo :: TruthTable -> IO()
sendInfo tt = do
    printTT tt
    printSendTT tt

processGate :: KeyPair -> IO KeyPair
processGate (Gate t k1 k2) = do
    a <- processGate k1
    b <- processGate k2
    o <- genKeyPair
    let tt = getTT t o a b
    sendInfo tt
    return o
    where
    helper o1 o2 o3 o4 (Input (a0, a1)) (Input (b0, b1))=
        TruthTable (a0, b0, o1) (a0, b1, o2) (a1, b0, o3) (a1, b1, o4)
    getTT AND (Input (o0, o1)) = helper o0 o0 o0 o1
    getTT OR (Input (o0, o1)) = helper o0 o1 o1 o1
    getTT XOR (Input (o0, o1)) = helper o0 o1 o1 o0
    getTT NAND (Input (o0, o1)) = helper o1 o1 o1 o0

processGate (Input a) = return (Input a)



(.&.) :: KeyPair -> KeyPair -> KeyPair
(.&.) = Gate AND
(.|.) :: KeyPair -> KeyPair -> KeyPair
(.|.) = Gate OR
xor :: KeyPair -> KeyPair -> KeyPair
xor = Gate XOR 
nand :: KeyPair -> KeyPair -> KeyPair
nand = Gate NAND

ifThenElse :: KeyPair -> KeyPair -> KeyPair -> KeyPair
ifThenElse bool tb fb = 
    let nbool = Gate NAND bool bool in
    ((bool .&. tb) .|. (nbool .&. fb))


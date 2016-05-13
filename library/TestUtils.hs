{-# LANGUAGE RankNTypes #-}
module TestUtils where

import Network.Socket
import qualified Consumer as C
import qualified Producer as P
import Control.Concurrent.Async
import Types
import Gate
import Data.Bits

doTest :: FiniteBits b => (Socket, Socket) -> (b, b) -> (forall a. SecureFunction a) -> IO [Bool]
doTest (csoc, psoc) (inputProduce, inputConsume) test = do
    conOutHandle <- asyncBound $ C.doWithSocket csoc (inputProduce, inputConsume) test
    proOutHandle <- asyncBound $ P.doWithSocket psoc (inputProduce, inputConsume) test
    con <- wait conOutHandle
    pro <- wait proOutHandle
    if foldl (&&) True (zipWith (==) con pro)
        then return con
        else do
            putStrLn "Producer"
            print pro
            putStrLn ""
            putStrLn "Consumer"
            print con
            error "disagreement on answer"

printTest :: FiniteBits b => (Socket, Socket) -> (b, b) -> (forall a. SecureFunction a) -> IO ()
printTest (csoc, psoc) (a, b) test = do
    putStrLn "Starting Test"
    countGates (finiteBitSize a) test
    conOutHandle <- asyncBound $ C.doWithSocket csoc (a, b) test
    proOutHandle <- asyncBound $ P.doWithSocket psoc (a, b) test
    conOut <- wait conOutHandle
    proOut <- wait proOutHandle
    print conOut
    print proOut
    putStrLn ""

{-# LANGUAGE RankNTypes #-}
module TestUtils where

import qualified Consumer                 as C
import           Control.Concurrent.Async
import           Data.Bits
import           Gate
import           Network.Socket
import qualified Producer                 as P
import           Types

doTest :: FiniteBits b => (Socket, Socket) -> (b, b) -> (forall a. SecureFunction) -> IO [Bool]
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

printTest :: FiniteBits b => (Socket, Socket) -> (b, b) -> (forall a. SecureFunction) -> IO ()
printTest (csoc, psoc) (a, b) test = do
    putStrLn "Starting Test"
--    countGates (finiteBitSize a) test -- doesnt work?
    conOutHandle <- asyncBound $ C.doWithSocket csoc (a, b) test
    proOutHandle <- asyncBound $ P.doWithSocket psoc (a, b) test
    conOut <- wait conOutHandle
    proOut <- wait proOutHandle
    print conOut
    print proOut
    putStrLn ""

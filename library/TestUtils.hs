{-# LANGUAGE RankNTypes #-}
module TestUtils where

import Network.Socket
import qualified Consumer as C
import qualified Producer as P
import Control.Concurrent.Async
import Types
import Data.Bits

doTest :: FiniteBits b => (Socket, Socket) -> (b, b) -> (forall a. SecureFunction a) -> IO [Bool]
doTest (csoc, psoc) (inputProduce, inputConsume) test = do
    conOutHandle <- asyncBound $ C.doWithSocket csoc (inputProduce, inputConsume) test
    proOutHandle <- asyncBound $ P.doWithSocket psoc (inputProduce, inputConsume) test
    con <- wait conOutHandle
    pro <- wait proOutHandle
    if foldl (&&) True (zipWith (==) con pro)
        then return con
        else error "disagreement on answer"

printTest :: FiniteBits b => (Socket, Socket) -> (b, b) -> (forall a. SecureFunction a) -> IO ()
printTest (csoc, psoc) (a, b) test = do
    conOutHandle <- asyncBound $ C.doWithSocket csoc (a, b) test
    proOutHandle <- asyncBound $ P.doWithSocket psoc (a, b) test
    conOut <- wait conOutHandle
    proOut <- wait proOutHandle
    print conOut
    print proOut
    putStrLn ""


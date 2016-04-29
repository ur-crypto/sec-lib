import Prelude 
import qualified Consumer as C
import qualified Producer as P
import Control.Concurrent.Async
import Ops
import Examples
import TestUtils
import System.Environment
import Data.Int

usage :: IO()
usage = putStrLn "Enter producer or consumer"

doArgs :: [String] -> IO()
doArgs ("producer":_) = do 
    res <- P.doWithoutSocket (test64, testb64) hammingDist
    print res
doArgs ("consumer":_) = do 
    res <- C.doWithoutSocket (test64, testb64) hammingDist 
    print res
doArgs ("both":_) = do
    hcsoc <- async C.getSocket
    hpsoc <- async P.getSocket
    csoc <- wait hcsoc
    psoc <- wait hpsoc
    printTest (csoc, psoc) (1::Int8,4::Int8) (<.)
    printTest (csoc, psoc) (2::Int8,4::Int8) (<.)
    printTest (csoc, psoc) (3::Int8,4::Int8) (<.)
    printTest (csoc, psoc) (4::Int8,4::Int8) (<.)
    printTest (csoc, psoc) (5::Int8,4::Int8) (<.)
    printTest (csoc, psoc) (6::Int8,4::Int8) (<.)
    printTest (csoc, psoc) (7::Int8,4::Int8) (<.)
    printTest (csoc, psoc) (8::Int8,4::Int8) (<.)
    printTest (csoc, psoc) (test32,testb32) (<.)
    printTest (csoc, psoc) (test32,test32) (<.)
    printTest (csoc, psoc) (testb32,test32) (<.)
    printTest (csoc, psoc) (test32, test32-1) (<.)
    printTest (csoc, psoc) (test32, test32) (<.)
    printTest (csoc, psoc) (test32, test32+1) (<.)
doArgs _ = usage

main :: IO()
main = do
    args <- getArgs
    doArgs args

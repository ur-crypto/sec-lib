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
    printTest (csoc, psoc) (test32,testb32) addInt
doArgs _ = usage

main :: IO()
main = do
    args <- getArgs
    doArgs args

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
    res <- P.doWithoutSocket (test8, testb8) edist 
    print res
doArgs ("consumer":_) = do 
    res <- C.doWithoutSocket (test8, testb8) edist
    print res
doArgs ("both":_) = do
    hcsoc <- async C.getSocket
    hpsoc <- async P.getSocket
    csoc <- wait hcsoc
    psoc <- wait hpsoc
    printTest (csoc, psoc) (1::Int8,4::Int8) (+)
doArgs _ = usage

main :: IO()
main = do
    args <- getArgs
    doArgs args

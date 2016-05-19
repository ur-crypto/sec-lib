import qualified Consumer                 as C
import           Control.Concurrent.Async
import           Data.Bits
import           Data.Int
import           Examples
import           Ops
import           Prelude
import qualified Producer                 as P
import           System.Environment
import           TestUtils

usage :: IO()
usage = putStrLn "Enter producer or consumer"

doArgs :: [String] -> IO()
doArgs ("producer":_) = do
    res <- P.doWithoutSocket (test8, testb8) (editDist 2 2)
    print res
doArgs ("consumer":_) = do
    res <- C.doWithoutSocket (test8, testb8) (editDist 2 2)
    print res
doArgs ("both":_) = do
    hcsoc <- async C.getSocket
    hpsoc <- async P.getSocket
    csoc <- wait hcsoc
    psoc <- wait hpsoc
    --printTest (csoc, psoc) (testmax, testmin) (hammingDistEff)
    printTest (csoc, psoc) (1::Int8,3::Int8) (myEditDist 2 1)
    printTest (csoc, psoc) (1::Int8,3::Int8) (myEditDist 1 2)
    printTest (csoc, psoc) (1::Int8,3::Int8) (myEditDist 2 2)
    printTest (csoc, psoc) (1::Int8,3::Int8) (myEditDist 3 1)
    printTest (csoc, psoc) (1::Int8,3::Int8) (myEditDist 1 3)
    printTest (csoc, psoc) (1::Int8,3::Int8) (myEditDist 4 1)
doArgs _ = usage

main :: IO()
main = do
    args <- getArgs
    doArgs args

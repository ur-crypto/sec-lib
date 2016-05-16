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
    res <- P.doWithoutSocket (test64, testb64) editDist
    print res
doArgs ("consumer":_) = do
    res <- C.doWithoutSocket (test64, testb64) editDist
    print res
doArgs ("both":_) = do
    hcsoc <- async C.getSocket
    hpsoc <- async P.getSocket
    csoc <- wait hcsoc
    psoc <- wait hpsoc
    printTest (csoc, psoc) (1::Int8,3::Int8) editDist
doArgs _ = usage

main :: IO()
main = do
    args <- getArgs
    doArgs args

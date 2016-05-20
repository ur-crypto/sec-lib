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
    res <- P.doWithoutSocket (test8, testb8) (myEditDist 4 1)
    print res
doArgs ("consumer":_) = do
    res <- C.doWithoutSocket (test8, testb8) (myEditDist 4 1)
    print res
doArgs ("both":a) = do
    hcsoc <- async C.getSocket
    hpsoc <- async P.getSocket
    csoc <- wait hcsoc
    psoc <- wait hpsoc
    let test = editDist
    case a of
      [] -> printTest (csoc, psoc) (1::Int8,3::Int8) (test 2 2)
      [x] -> printTest (csoc, psoc) (1::Int8,3::Int8) (test (read x)  (read x))
      x:y:_ -> printTest (csoc, psoc) (1::Int8,3::Int8) (test (read x) (read y))

doArgs _ = usage

main :: IO()
main = do
    args <- getArgs
    doArgs args

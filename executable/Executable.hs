import Prelude 
import qualified Consumer as C
import qualified Producer as P
import Control.Concurrent.Async
import Ops
import Examples
import TestUtils
import System.Environment
import Data.Int
    
data Mode = Producer | Consumer | Both

usage :: IO()
usage = putStrLn "Enter producer or consumer"

parseArgs :: [String] -> Maybe Mode
parseArgs ("producer":_) = Just Producer
parseArgs ("consumer":_) = Just Consumer
parseArgs ("both":_) = Just Both
parseArgs [] = Nothing
parseArgs _ = Nothing

doArgs :: Maybe Mode -> IO()
doArgs (Just Producer) = do 
    res <- P.doWithoutSocket (test64, testb64) hammingDist 
    print res
doArgs (Just Consumer) = do 
    res <- C.doWithoutSocket (test64, testb64) hammingDist
    print res
doArgs (Just Both) = do
    hcsoc <- async C.getSocket
    hpsoc <- async P.getSocket
    csoc <- wait hcsoc
    psoc <- wait hpsoc
    printTest (csoc, psoc) (3 :: Int8, 7 :: Int8) andShift
doArgs Nothing = usage

main :: IO()
main = do
    args <- getArgs
    doArgs $ parseArgs args

import Prelude 
import Utils
import qualified Consumer as C
import qualified Producer as P
import TestUtils
import Control.Concurrent.Async
import Examples
import System.Environment

    
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
    (res0, res1) <- P.doWithoutSocket (testb64, test64) numPltC
    printKey (Just False) res0
    printKey (Just True) res1
    return ()
doArgs (Just Consumer) = do 
    res <- C.doWithoutSocket (testb64, test64) numPltC
    printKey Nothing res
    return ()
doArgs (Just Both) = do
    hcsoc <- async C.getSocket
    hpsoc <- async P.getSocket
    csoc <- wait hcsoc
    psoc <- wait hpsoc
    conOutHandle <- asyncBound $ C.doWithSocket csoc (test16, test16) numCmp
    proOutHandle <- asyncBound $ P.doWithSocket psoc (test16, test16) numCmp
    conOut <- wait conOutHandle
    (proOut0, proOut1) <- wait proOutHandle
    printKey Nothing conOut
    printKey (Just False) proOut0
    printKey (Just True) proOut1
    return ()


doArgs Nothing = usage

main :: IO()
main = do
    args <- getArgs
    doArgs $ parseArgs args

import Prelude 
import qualified Consumer as C
import qualified Producer as P
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
    res <- P.doWithoutSocket (testb64, test64) numCmp
    print res
    return ()
doArgs (Just Consumer) = do 
    res <- C.doWithoutSocket (testb64, test64) numCmp
    print res
    return ()
doArgs (Just Both) = do
    hcsoc <- async C.getSocket
    hpsoc <- async P.getSocket
    csoc <- wait hcsoc
    psoc <- wait hpsoc
    conOutHandle <- asyncBound $ C.doWithSocket csoc (test64, testb64) numCmp
    proOutHandle <- asyncBound $ P.doWithSocket psoc (test64, testb64) numCmp
    conOut <- wait conOutHandle
    proOut <- wait proOutHandle
    print conOut
    print proOut
    return ()


doArgs Nothing = usage

main :: IO()
main = do
    args <- getArgs
    doArgs $ parseArgs args

import Prelude 
import Utils
import qualified Consumer as C
import qualified Producer as P
import Examples
import System.Environment

    
data Mode = Producer | Consumer

usage :: IO()
usage = putStrLn "Enter producer or consumer"

parseArgs :: [String] -> Maybe Mode
parseArgs ("producer":_) = Just Producer
parseArgs ("consumer":_) = Just Consumer
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
doArgs Nothing = usage

main :: IO()
main = do
    args <- getArgs
    doArgs $ parseArgs args

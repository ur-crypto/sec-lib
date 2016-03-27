import Prelude 
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
    _ <- P.produceMain (testNum, testNum) numCmp
    return ()
doArgs (Just Consumer) = do 
    _ <- C.consumeMain (testNum, testNum) numCmp
    return ()
doArgs Nothing = usage

main :: IO()
main = do
    args <- getArgs
    doArgs $ parseArgs args

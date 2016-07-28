{-# LANGUAGE TemplateHaskell #-}
import qualified Consumer                 as C
import           Control.Concurrent.Async

import           Data.Bits
import           Data.Int
import           Ops
import           Pipes
import qualified Pipes.Prelude            as PP
import qualified Producer                 as P
import           System.Environment
import           TestUtils


usage :: IO()
usage = putStrLn "Enter producer or consumer"

doArgs :: [String] -> IO()
doArgs ("producer":_) = runEffect $ P.doWithoutSocket (test8, testb8) (==.) >-> PP.print
doArgs ("consumer":_) = runEffect $ C.doWithoutSocket (test8, testb8) (==.) >-> PP.print
doArgs ("both":a) = doBoth

doArgs _ = usage

doBoth :: IO ()
doBoth = do
    hcsoc <- async C.getSocket
    hpsoc <- async P.getSocket
    csoc <- wait hcsoc
    psoc <- wait hpsoc
    printTest (csoc, psoc) (1::Int8,3::Int8) (.&.)
main :: IO()
main = do
    args <- getArgs
    doArgs args

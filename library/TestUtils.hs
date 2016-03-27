{-# LANGUAGE RankNTypes #-}
module TestUtils where

import Network.Socket
import qualified Consumer as C
import qualified Producer as P
import Control.Concurrent.Async
import Types

doTest :: (Socket, Socket) -> (Int, Int) -> (forall a. TestBool a) -> IO Bool
doTest (csoc, psoc) (inputProduce, inputConsume) test = do
    conOutHandle <- asyncBound $ C.doWithSocket csoc (inputProduce, inputConsume) test
    proOutHandle <- asyncBound $ P.doWithSocket psoc (inputProduce, inputConsume) test
    conOut <- wait conOutHandle
    (_, proOut1) <- wait proOutHandle
    return $ conOut == proOut1

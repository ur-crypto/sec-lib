module TestUtils where

import           Pipes
import qualified Pipes.Prelude  as PP

import qualified Consumer       as C

import           Data.Bits
import           Data.Int
import           Network.Socket
import qualified Producer       as P
import           Types

test8 :: Int8
test8 = 35

testb8 :: Int8
testb8 = 74

test16 :: Int16
test16 = 125

testb16 :: Int16
testb16 = 200

test32 :: Int32
test32 = 39393


testb32 :: Int32
testb32 = 120200

testb64 :: Int64
testb64 = 744073709551616

test64 :: Int64
test64 = 395648674974903

testmax :: Int64
testmax = 99223372036854775807

testmin :: Int64
testmin = -9224472036854775808

doTest :: FiniteBits b => (Socket, Socket) -> (b, b) -> (SecureFunction) -> Producer Bool IO ()
doTest (csoc, psoc) (inputProduce, inputConsume) test = do

  liftIO $ putStrLn "doing tests"
  let consumer = C.doWithSocket csoc (inputProduce, inputConsume) test
  let producer = P.doWithSocket psoc (inputProduce, inputConsume) test
  (PP.zip consumer producer) >-> do
    (con, pro) <- await
    if (con == pro)
      then yield con
      else error $
           "\nProducer\n" ++
           show pro ++
           "\nConsumer\n" ++
           show con ++
           "\nDisagreement on answer"

printTest :: FiniteBits b => (Socket, Socket) -> (b, b) -> SecureFunction -> IO ()
printTest socs nums test = do
  runEffect $ doTest socs nums test >-> PP.print

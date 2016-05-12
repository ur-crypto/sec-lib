{-# LANGUAGE RankNTypes #-}
import qualified Test.Tasty
import Test.Tasty.Hspec
import Control.Concurrent.Async
import Examples
import TestUtils
import Types
import Utils
import qualified Ops as O
import Network.Socket
import qualified Producer as P
import qualified Consumer as C
import Data.Int
import Data.Bits

main :: IO ()
main = do
    putStrLn "Attempting to get sockets"
    hcsoc <- async C.getSocket
    hpsoc <- async P.getSocket
    putStrLn "Waiting on sockets"
    csoc <- wait hcsoc
    psoc <- wait hpsoc
    putStrLn "Creating Tests"
    test <- testSpec "lazy-circuits" (spec (csoc, psoc))
    putStrLn "Entering Test Suite"
    Test.Tasty.defaultMain test

spec :: (Socket, Socket) -> Spec
spec (csoc, psoc)=  do
{-
    it "Tests Edit Distance" $ do
        let(x,y) = (10::Int16, 20::Int16) 
        ans <- doTest (csoc, psoc) (x, y) levenshtein2
        ans `shouldBe` (bits2Bools $ boolsEditDistance (bits2Bools x) (bits2Bools y))
-}
    it "Num Eq 16 True" $ boolTest numEq test16 test16 True
    it "Num Eq 16 True" $ boolTest numEq test16 test16 True
    it "Num Eq 16 False" $ boolTest numEq test16 (test16-1) False
    it "Num Eq 32 True" $ boolTest numEq test32 test32 True
    it "Num Eq 32 False" $ boolTest numEq test32 (test32-1) False
    it "Num Eq 64 True" $ boolTest numEq test64 test64 True
    it "Num Eq 64 False" $ boolTest numEq test64 (test64-1) False
    it "Num Cmp 64 True" $ boolTest (O.<.) test64 (test64-1) True
    it "Num Cmp 64 False" $ boolTest (O.<.) test64 test64 False
    it "Num Cmp 64 False" $ boolTest (O.<.) test64 (test64+1) False
    it "Num hamDist 64 False" $ hamTest hammingDist test64 testb64 (28 :: Int8)
    it "Num XOR 64 True" $ listTest xor (15 :: Int64) (20 :: Int64) (xor (15 :: Int64) (20 :: Int64))
    it "Num OR 64 True" $ listTest (.|.) (15 :: Int64) (20 :: Int64) ((.|.) (15 :: Int64) (20 :: Int64))
    it "Num nand 64 True" $ listTest (O..~&.) (15 :: Int64) (20 :: Int64) ((\x -> \y -> (complement (x .&. y))) (15 :: Int64) (20 :: Int64))
    --it "Num Shift 64 True" $ listTest andShift (15 :: Int64) (20 :: Int64) ((.&.) (30 :: Int64) (20 :: Int64))
    it "Tests for constant negation" $ 
        listTest (\x -> \y -> (complement $ (shiftL x 2) .&. (shiftL y 2)))
            (2 ::Int8) (3 ::Int8) (complement $ (shiftL (2::Int8) 2) .&. (shiftL (3::Int8) 2))
    it "Test addition" $
        listTest (+) (1::Int16) (1::Int16) (2::Int16)
   -- it "Num Cmp 8 All" $ exhaustiveTest (O.==.) (==.)
    it "Sockets Close" $ do
        close csoc
        close psoc
        True `shouldBe` True
    where
        boolTest :: FiniteBits b => (forall a. SecureFunction a) -> b -> b -> Bool -> Expectation
        boolTest test num1 num2 expect = (doTest (csoc, psoc) (num1, num2) test) `shouldReturn` [expect]

        hamTest  :: FiniteBits b => (forall a. SecureFunction a) -> b -> b -> Int8 -> Expectation
        hamTest test inputNum inputNum2 expectedNumber = do 
            ourAnswers <- (doTest (csoc, psoc) (inputNum, inputNum2) test) 
            let expectAnswers = (bits2Bools expectedNumber)
            ourAnswers `shouldBe` expectAnswers
     

        listTest  :: FiniteBits b => (forall a. SecureFunction a) -> b -> b -> b -> Expectation
        listTest test inputNum inputNum2 expectedNumber = do 
            ourAnswers <- (doTest (csoc, psoc) (inputNum, inputNum2) test) 
            let expectAnswers = (bits2Bools expectedNumber)
            ourAnswers `shouldBe` expectAnswers

        {-
        exhaustiveTest :: (forall a. SecureFunction a) -> (forall b. FiniteBits b => b -> b -> b) -> Expectation
        exhaustiveTest test equivalent = do
          let nums = [minBound :: Int8 .. maxBound :: Int8]
          let numCombos = [ (x, y) | x<-nums, y<-nums ] --idk how this works but it sure is pretty
          let numAnswers = map (\xt -> case xt of (x, y) -> bits2Bools $ (equivalent) x y) numCombos
          ourAnswers <- mapM (\x -> doTest (csoc, psoc) x test) numCombos
          ourAnswers `shouldBe` numAnswers
        -}

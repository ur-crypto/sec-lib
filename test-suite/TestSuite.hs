{-# LANGUAGE RankNTypes #-}
import qualified Test.Tasty
import Test.Tasty.Hspec
import Control.Concurrent.Async
import Examples
import TestUtils
import Types
import Network.Socket
import qualified Producer as P
import qualified Consumer as C
import Data.Int
import Data.Bits

main :: IO ()
main = do
    hcsoc <- async C.getSocket
    hpsoc <- async P.getSocket
    csoc <- wait hcsoc
    psoc <- wait hpsoc
    test <- testSpec "lazy-circuits" (spec (csoc, psoc))
    Test.Tasty.defaultMain test

spec :: (Socket, Socket) -> Spec
spec (csoc, psoc)=  do
    it "Num Eq 16 True" $ boolTest numEq test16 test16 True
    it "Num Eq 16 True" $ boolTest numEq test16 test16 True
    it "Num Eq 16 False" $ boolTest numEq test16 (test16-1) False
    it "Num Eq 32 True" $ boolTest numEq test32 test32 True
    it "Num Eq 32 False" $ boolTest numEq test32 (test32-1) False
    it "Num Eq 64 True" $ boolTest numEq test64 test64 True
    it "Num Eq 64 False" $ boolTest numEq test64 (test64-1) False

    it "Num Cmp 64 True" $ boolTest numCmp test64 (test64-1) True
    it "Num Cmp 64 False" $ boolTest numCmp test64 test64 False
    it "Num Cmp 64 False" $ boolTest numCmp test64 (test64+1) False

    it "Num Cmp 8 All" $ do
        let nums = [minBound :: Int8 .. maxBound :: Int8] 
        let numCombos = [ (x, y) | x<-nums, y<-nums ] --idk how this works but it sure is pretty
        let numAnswers = map (\xt -> case xt of (x, y) -> [x < y]) numCombos
        ourAnswers <- mapM (\x -> doTest (csoc, psoc) x numCmp) numCombos
        ourAnswers `shouldBe` numAnswers
    it "Sockets Close" $ do
        close csoc
        close psoc
        True `shouldBe` True
    where
    boolTest :: FiniteBits b => (forall a. SecureFunction a) -> b -> b -> Bool -> Expectation
    boolTest test num1 num2 expect = (doTest (csoc, psoc) (num1, num2) test) `shouldReturn` [expect]

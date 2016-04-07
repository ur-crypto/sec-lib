import qualified Test.Tasty
import Test.Tasty.Hspec
import Control.Concurrent.Async
import Examples
import TestUtils
import Network.Socket
import qualified Producer as P
import qualified Consumer as C
import Data.Int

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
    it "Num Cmp 16 True" $ do
        res <- doTest (csoc, psoc) (test16, test16) numCmp
        res `shouldBe` True
    it "Num Cmp 16 False" $ do
        res <- doTest (csoc, psoc) (test16, test16-1) numCmp
        res `shouldBe` False
    it "Num Cmp 32 True" $ do
        res <- doTest (csoc, psoc) (test32, test32) numCmp
        res `shouldBe` True
    it "Num Cmp 32 False" $ do
        res <- doTest (csoc, psoc) (test32, test32-1) numCmp
        res `shouldBe` False
    it "Num Cmp 64 True" $ do
        res <- doTest (csoc, psoc) (test64, test64) numCmp
        res `shouldBe` True
    it "Num Cmp 64 False" $ do
        res <- doTest (csoc, psoc) (test64, test64-1) numCmp
        res `shouldBe` False
    it "Num Gt 64 ?" $ do
        res <- doTest (csoc, psoc) (test64, test64) numPltC
        res `shouldBe` False
    it "Num GT 64 ? " $ do
        res <- doTest (csoc, psoc) (test64-1, test64) numPltC
        res `shouldBe` True
    it "Num GT 64 ?" $ do
        res <- doTest (csoc, psoc) (test64, test64-1) numPltC
        res `shouldBe` False
    it "Num Cmp 8 All" $ do
        let nums = [minBound :: Int8 .. maxBound :: Int8] 
        let numCombos = [ (x, y) | x<-nums, y<-nums ] --idk how this works but it sure is pretty
        let numAnswers = map (\xt -> case xt of (x, y) -> x == y) numCombos
        ourAnswers <- mapM (\x -> doTest (csoc, psoc) x numCmp) numCombos
        ourAnswers `shouldBe` numAnswers
    it "Sockets Close" $ do
        close csoc
        close psoc
        True `shouldBe` True


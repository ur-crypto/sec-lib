{-# LANGUAGE RankNTypes #-}
import qualified Test.Tasty
import Test.Tasty.Hspec
import Control.Concurrent.Async
import Examples
import Types
import Network.Socket
import qualified Producer as P
import qualified Consumer as C

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
    it "Num Cmp True" $ do
        res <- doTest (csoc, psoc) (testNum, testNum) numCmp
        res `shouldBe` True
    it "Num Cmp False" $ do
        res <- doTest (csoc, psoc) (testNum, testNum-1) numCmp
        res `shouldBe` False
    it "Sockets Close" $ do
        close csoc
        close psoc
        True `shouldBe` True

doTest :: (Socket, Socket) -> (Int, Int) -> (forall a. TestBool a) -> IO Bool
doTest (csoc, psoc) (inputProduce, inputConsume) test = do
    conOutHandle <- asyncBound $ C.doWithSocket csoc (inputProduce, inputConsume) test
    proOutHandle <- asyncBound $ P.doWithSocket psoc (inputProduce, inputConsume) test
    conOut <- wait conOutHandle
    (_, proOut1) <- wait proOutHandle
    return $ conOut == proOut1

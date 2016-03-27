{-# LANGUAGE RankNTypes #-}
import qualified Test.Tasty
import Test.Tasty.Hspec
import Control.Concurrent.Async
import Examples
import Types
import qualified Producer as P
import qualified Consumer as C

main :: IO ()
main = do
    test <- testSpec "lazy-circuits" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec =  do
    it "is trivially true" $
        True `shouldBe` True
    it "Num Compare True" $ do
        res <- doTest (testNum, testNum) numCmp True
        res `shouldBe` True
    it "Num Compare False" $ do
        res <- doTest (testNum, testNum - 1) numCmp False
        res `shouldBe` True

doTest :: (Int, Int) -> (forall a. TestBool a) -> Bool -> IO Bool
doTest (inputProduce, inputConsume) test expect = do
    conOutHandle <- async $ C.consumeMain (inputProduce, inputConsume) test
    proOutHandle <- async $ P.produceMain (inputProduce, inputConsume) test
    conOut <- wait conOutHandle
    (_, proOut1) <- wait proOutHandle
    return $  (conOut == proOut1) == expect

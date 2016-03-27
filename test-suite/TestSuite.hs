import qualified Test.Tasty
import Test.Tasty.Hspec
import Control.Concurrent.Async
import Examples
import TestUtils
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


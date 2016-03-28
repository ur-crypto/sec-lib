import Criterion
import qualified Criterion.Main
import Control.Concurrent.Async
import Examples
import TestUtils
import qualified Producer as P
import qualified Consumer as C

main :: IO ()
main = do
    hcsoc <- async C.getSocket
    hpsoc <- async P.getSocket
    csoc <- wait hcsoc
    psoc <- wait hpsoc
    Criterion.Main.defaultMain [
        bgroup "32 Bit Number Compare"
            [ bench "Fill" $ nfIO (doTest (csoc, psoc) (testNum, testNum) numCmp)
            , bench "True" $ nfIO (doTest (csoc, psoc) (testNum, testNum) numCmp)
            , bench "False" $ nfIO (doTest (csoc, psoc) (testNum, testNum-1) numCmp)
            ]
        ]


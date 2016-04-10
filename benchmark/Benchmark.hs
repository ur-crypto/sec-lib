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
        bgroup "Integer Compare"
            [ bench "8" $ nfIO (doTest (csoc, psoc) (test8, test8) numEq)
            , bench "16" $ nfIO (doTest (csoc, psoc) (test16, test16) numEq)
            , bench "32" $ nfIO (doTest (csoc, psoc) (test32, test32) numEq)
            , bench "64" $ nfIO (doTest (csoc, psoc) (test64, test64) numEq)
            ]
        ]


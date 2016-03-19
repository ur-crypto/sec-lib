import Criterion
import qualified Criterion.Main

main :: IO ()
main = Criterion.Main.defaultMain [bench "const" (whnf const ())]

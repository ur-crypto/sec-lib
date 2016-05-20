module Counter where
import           Data.Bits
import           Data.List
import qualified Data.Map       as M
import           Network.Socket
import           Types

countGates :: FiniteBits a => Socket -> (a, a) -> SecureFunction -> IO KeyType
countGates ignored (con, pro) func = do
  let pn = [1..finiteBitSize pro]
      cn = [1..finiteBitSize con]
      pcounts = map (const $ Input ignored [] (return (Counter 0 0 0 0, M.empty))) pn
      qcounts = map (const $ Input ignored [] (return (Counter 0 0 0 0, M.empty))) cn
      results = func pcounts qcounts
      extract Input {value = a} = a
      extract (Constant _) = return (Counter 0 0 0 0, M.empty)
      ioCounts = map extract results
  counts <- sequence ioCounts
  let accum (Counter a1 a2 a3 a4) (Counter b1 b2 b3 b4, _) = Counter (a1+b1) (a2+b2) (a3+b3) (a4+b4)
      finalCount = foldl accum (Counter 0 0 0 0) counts
  return finalCount

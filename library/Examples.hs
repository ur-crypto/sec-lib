module Examples where
import Types
import Utils

testNum :: Int
testNum = 1234567890

numCmp :: [Value a] -> [Value a] -> Value a
numCmp n1 n2 = foldl1 (.&.) (zipWith bij n1 n2)


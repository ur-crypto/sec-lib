module Utils where
import OpenSSL.Random
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.LargeWord as LW (Word128, LargeKey(..))
import Data.Binary.Get

getKey :: IO BS.ByteString
getKey = randBytes 8

genKeyPair :: IO (BS.ByteString, BS.ByteString)
genKeyPair = do
    k1 <- randBytes 16
    k2 <- randBytes 16
    return (k1, k2)

bsToWord128 :: BS.ByteString -> LW.Word128
bsToWord128 s = let (s1, s2) = BS.splitAt 64 s in
    let a1 = runGet getWord64le s2 in
    let a2 = runGet getWord64le s1 in
    a1

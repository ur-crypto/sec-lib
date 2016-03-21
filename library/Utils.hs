module Utils where
import OpenSSL.Random
import Data.Word
import Data.ByteString

getKey :: IO ByteString
getKey = randBytes 8
    

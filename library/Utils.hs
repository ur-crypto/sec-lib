{-# LANGUAGE NamedFieldPuns #-}
module Utils where
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Data.Binary
import           Data.Bits
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Int
import           System.Entropy
import           Text.Bytedump

import           Types

-- In Bytes
--

cipherSize :: Int64
cipherSize = 16

--DO NOT CHANGE
keyLength :: Int64
keyLength = 15

padLength :: Int64
padLength = cipherSize - keyLength

--END DO NOT CHANGE

fromBinary :: (Binary a, Binary b) => a -> b
fromBinary = decode . encode

genFixedKey :: IO Key
genFixedKey = getEntropy (fromIntegral cipherSize) >>= return . fromBinary

genRootKey :: IO Key
genRootKey = do
  a <- getEntropy (fromIntegral cipherSize)
  let (Just (as, al)) = BS.unsnoc a
  return $ decode $ encode $ BS.snoc as (setBit al 0)

mkKeyPairFromKey :: Key -> Key -> Bool -> (Key, Key)
mkKeyPairFromKey rkey k0 sw =
  let
    k1 = decode $ encode $ xor k0 rkey
    in
    if sw
      then (k1, k0)
      else (k0, k1)

genKeyPair :: Key -> IO (Key, Key)
genKeyPair rkey = do
    rnd <- getEntropy (fromIntegral cipherSize) >>= return . fromBinary
    return $ mkKeyPairFromKey rkey rnd False

getAESKeys :: Key -> Key -> (AES128, AES128)
getAESKeys a b = (initFixedKey a, initFixedKey b)

initFixedKey :: Key -> AES128
initFixedKey k = throwCryptoError $ cipherInit (fromBinary k :: BS.ByteString)

hash :: AES128 -> Key -> Key
hash k v = decode $ LBS.fromStrict $ ecbEncrypt k $ decode $ encode v

hashPair :: AES128 -> Key -> Key -> Key
hashPair fkey a b = xor (hash fkey a) (hash fkey b)

enc :: AES128 -> (Key, Key, Key) -> Key
enc fkey (a, b, o) = xor o $ hashPair fkey a b

keyString :: Key -> String
keyString = dumpBS . fromBinary

printKey :: Maybe Bool -> Key -> IO()
printKey (Just False) key = putStrLn $ "0:\t" ++ keyString key
printKey (Just True) key = putStrLn $ "1:\t" ++ keyString key
printKey Nothing key = putStrLn $ "?:\t" ++ keyString key

bits2Bools :: FiniteBits a => a -> [Bool]
bits2Bools i = reverse $ map (testBit i) [0..(finiteBitSize i - 1) :: Int]

bsToBools :: BS.ByteString -> [Bool]
bsToBools bs = concatMap bits2Bools $ BS.unpack bs

numBytes :: FiniteBits a => a -> Int
numBytes n = finiteBitSize n `quot` 8


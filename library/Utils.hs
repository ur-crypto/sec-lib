{-# LANGUAGE NamedFieldPuns #-}
module Utils where
import           Control.Monad.State.Lazy
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Data.Bits
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.Int
import           Data.Word
import           System.Entropy
import           Text.Bytedump
import           Types


processOutputs :: LBS.ByteString -> [Literal] -> [Either KeyType Bool] -> (LBS.ByteString, [Either KeyType Bool])
processOutputs lastState [] accum = (lastState, accum)
processOutputs initialState (x:xs) accum =
  case x of
    Constant b' -> processOutputs initialState xs (accum ++ [Right b'])
    Input{keyState} ->
      let (resultValue, resultState) = runState keyState initialState in
        processOutputs resultState xs (accum ++ [Left resultValue])

-- In Bytes
--

cipherSize :: Int64
cipherSize = 16

--DO NOT CHANGE
keyLength :: Int64
keyLength = 15

padLength :: Int64
padLength = cipherSize - keyLength

zeros :: BS.ByteString
zeros = getFill False

ones :: BS.ByteString
ones = getFill True

getFill :: Bool -> BS.ByteString
getFill x = BS.pack $ map (const (toEnum $ fromEnum x :: Word8)) [1 .. padLength]

getFills :: Bool -> (BS.ByteString, BS.ByteString)
getFills x = (getFill x, getFill $ not x)

genFixedKey :: IO BS.ByteString
genFixedKey = getEntropy (fromIntegral cipherSize)

genRootKey :: IO BS.ByteString
genRootKey = do
  a <- getEntropy (fromIntegral cipherSize)
  let (Just (as, al)) = BS.unsnoc a
  return $ BS.snoc as (setBit al 0)

mkKeyPairFromKey :: BS.ByteString -> BS.ByteString -> Bool -> (BS.ByteString, BS.ByteString)
mkKeyPairFromKey rkey k0 sw =
  let
    k1 = BS.pack $ BS.zipWith xor k0 rkey
    in
    if sw
      then (k1, k0)
      else (k0, k1)

genKeyPair :: BS.ByteString -> IO (BS.ByteString, BS.ByteString)
genKeyPair rkey = do
    rnd <- getEntropy (fromIntegral cipherSize)
    return $ mkKeyPairFromKey rkey rnd False

getAESKeys :: BS.ByteString -> BS.ByteString -> (AES128, AES128)
getAESKeys a b = (throwCryptoError $ cipherInit a :: AES128, throwCryptoError $ cipherInit b :: AES128)

initFixedKey :: BS.ByteString -> AES128
initFixedKey str = throwCryptoError $ cipherInit str

hash :: AES128 -> Key -> Key
hash = ecbEncrypt

hashPair :: AES128 -> Key -> Key -> Key
hashPair fkey a b = BS.pack $ BS.zipWith xor (hash fkey a) (hash fkey b)

enc :: AES128 -> (Key, Key, Key) -> Key
enc fkey (a, b, o) =
    BS.pack $ BS.zipWith xor o $ hashPair fkey a b

keyString :: BS.ByteString -> String
keyString = dumpBS

printKey :: Maybe Bool -> BS.ByteString -> IO()
printKey (Just False) key = putStrLn $ "0:\t" ++ keyString key
printKey (Just True) key = putStrLn $ "1:\t" ++ keyString key
printKey Nothing key = putStrLn $ "?:\t" ++ keyString key

bits2Bools :: FiniteBits a => a -> [Bool]
bits2Bools i = reverse $ map (testBit i) [0..(finiteBitSize i - 1) :: Int]

bsToBools :: BS.ByteString -> [Bool]
bsToBools bs = concatMap bits2Bools $ BS.unpack bs

numBytes :: FiniteBits a => a -> Int
numBytes n = finiteBitSize n `quot` 8
--

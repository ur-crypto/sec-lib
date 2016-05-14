module Utils where
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Data.Bits
import qualified Data.ByteString     as BS
import           Data.Word
import           System.Entropy
import           Text.Bytedump
import           Types

-- In Bytes
--

type FixedKey = AES128

cipherSize :: Int
cipherSize = 16

--DO NOT CHANGE
keyLength :: Int
keyLength = 15

padLength :: Int
padLength = cipherSize - keyLength

zeros :: BS.ByteString
zeros = getFill False

ones :: BS.ByteString
ones = getFill True

getFill :: Bool -> BS.ByteString
getFill x = BS.pack $ map (const (toEnum $ fromEnum x :: Word8)) [1 .. padLength]

getFills :: Bool -> (BS.ByteString, BS.ByteString)
getFills x = (getFill $ x, getFill $ not x)

genFixedKey :: IO BS.ByteString
genFixedKey = getEntropy cipherSize

genRootKey :: IO BS.ByteString
genRootKey = getEntropy keyLength

genKeyPair :: BS.ByteString -> IO (BS.ByteString, BS.ByteString)
genKeyPair rkey = do
    rnd <- getEntropy cipherSize
    let (Just (k0, fill)) = BS.unsnoc rnd
    let k1 = BS.pack $ BS.zipWith xor k0 rkey
    let (b0, b1) = getFills $ testBit fill 0
    return (BS.concat [k0, b0], BS.concat [k1, b1])

getAESKeys :: BS.ByteString -> BS.ByteString -> (AES128, AES128)
getAESKeys a b = (throwCryptoError $ cipherInit a :: AES128, throwCryptoError $ cipherInit b :: AES128)

initFixedKey :: BS.ByteString -> AES128
initFixedKey str = throwCryptoError $ cipherInit str

encOutKey :: AES128 -> (Key, Key, Key) -> Key
encOutKey fkey (a, b, o) =
    BS.pack $ BS.zipWith xor o . BS.pack $ BS.zipWith xor (ecbEncrypt fkey a) (ecbEncrypt fkey b)

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

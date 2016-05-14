module Utils where
import           Control.Monad
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Data.Array.IO
import           Data.Bits
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as D
import           Data.Word
import           System.Entropy
import           System.Random
import           Text.Bytedump

-- In Bytes
--

type FixedKey = AES128

cipherSize :: Int
cipherSize = 16

keyLength :: Int
keyLength = 10

padLength :: Int
padLength = cipherSize - keyLength

zeros :: BS.ByteString
zeros = BS.pack $ map (\_ -> 0 :: Word8) [1 .. padLength]

zeroBuilder :: D.Builder
zeroBuilder = D.byteString zeros

genFixedKey :: IO BS.ByteString
genFixedKey = do
    k <- getEntropy cipherSize
    return k

genRootKey :: IO BS.ByteString
genRootKey = do
    k <- getEntropy keyLength
    return k

genKeyPair :: BS.ByteString -> IO (BS.ByteString, BS.ByteString)
genKeyPair rkey = do
    k1 <- getEntropy keyLength
    let k2 = BS.pack $ BS.zipWith xor k1 rkey
    let k10 = BS.concat [k1, zeros]
    let k20 = BS.concat [k2, zeros]
    return (k10, k20)

getAESKeys :: BS.ByteString -> BS.ByteString -> (AES128, AES128)
getAESKeys a b = (throwCryptoError $ cipherInit a :: AES128, throwCryptoError $ cipherInit b :: AES128)

initFixedKey :: BS.ByteString -> AES128
initFixedKey str = throwCryptoError $ cipherInit str

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

encOutKey :: AES128 -> (BS.ByteString, BS.ByteString, BS.ByteString) -> BS.ByteString
encOutKey fkey (a, b, o) =
    BS.pack $ BS.zipWith (xor) o . BS.pack $ BS.zipWith (xor) (ecbEncrypt fkey a) (ecbEncrypt fkey b)

decOutKey :: AES128 -> (BS.ByteString, BS.ByteString, BS.ByteString) -> Maybe BS.ByteString
decOutKey fkey ktup =
    let check = encOutKey fkey ktup in
    let (_, z) = BS.splitAt keyLength check in
    if z == zeros
        then Just check
        else Nothing

keyString :: BS.ByteString -> String
keyString = dumpBS

printKey :: Maybe Bool -> BS.ByteString -> IO()
printKey (Just False) key = putStrLn $ "0:\t" ++ keyString key
printKey (Just True) key = putStrLn $ "1:\t" ++ keyString key
printKey Nothing key = putStrLn $ "?:\t" ++ keyString key

bits2Bools :: FiniteBits a => a -> [Bool]
bits2Bools i = reverse $ map (testBit i) [0..((finiteBitSize i) - 1) :: Int]

bsToBools :: BS.ByteString -> [Bool]
bsToBools bs = concatMap bits2Bools $ BS.unpack bs

numBytes :: FiniteBits a => a -> Int
numBytes n = (finiteBitSize n) `quot` 8

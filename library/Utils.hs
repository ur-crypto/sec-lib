module Utils where
import OpenSSL.Random
import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as D
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Codec.Binary.BubbleBabble as X
import Crypto.Error
import Data.ByteString.Internal (unpackBytes)


-- In Bytes

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

genKeyPair :: IO (BS.ByteString, BS.ByteString)
genKeyPair = do
    k1 <- randBytes keyLength
    k2 <- randBytes keyLength
    let k1Builder = mappend (D.byteString k1) zeroBuilder
    let k2Builder = mappend (D.byteString k2) zeroBuilder

    return (L.toStrict . D.toLazyByteString $ k1Builder, L.toStrict . D.toLazyByteString $ k2Builder)
    
getAESKeys :: BS.ByteString -> BS.ByteString -> (AES128, AES128)
getAESKeys a b = (throwCryptoError $ cipherInit a :: AES128, throwCryptoError $ cipherInit b :: AES128)

encOutKey :: (BS.ByteString, BS.ByteString, BS.ByteString) -> BS.ByteString
encOutKey (a, b, o) =
    let (ac, bc) = getAESKeys a b in 
    ecbEncrypt ac $ ecbEncrypt bc o

decOutKey :: (BS.ByteString, BS.ByteString, BS.ByteString) -> Maybe BS.ByteString
decOutKey (a, b, o) = 
    let (ac, bc) = getAESKeys a b in 
    let check = ecbDecrypt bc $ ecbDecrypt ac o in
    let (_, z) = BS.splitAt keyLength check in
    if z == zeros
        then Just check
        else Nothing
    

keyString :: BS.ByteString -> String
keyString key = X.encode . unpackBytes $ key

printKey :: Maybe Bool -> BS.ByteString -> IO()
printKey (Just False) key = putStrLn $ "0:\t" ++ keyString key
printKey (Just True) key = putStrLn $ "1:\t" ++ keyString key
printKey Nothing key = putStrLn $ "?:\t" ++ keyString key

bitsToBools :: FiniteBits a => a -> [Bool]
bitsToBools i = map (testBit i) [0..((finiteBitSize i) - 1) :: Int]

bsToBools :: BS.ByteString -> [Bool]
bsToBools bs = concatMap bitsToBools $ BS.unpack bs

numBytes :: FiniteBits a => a -> Int
numBytes n = (finiteBitSize n) `quot` 8

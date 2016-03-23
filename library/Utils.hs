module Utils where
import OpenSSL.Random
import Data.Word
import Data.Monoid
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as D
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error

zeroBuilder :: D.Builder
zeroBuilder = mappend (D.word32LE 0) (D.word16LE 0)

genKeyPair :: IO (BS.ByteString, BS.ByteString)
genKeyPair = do
    k1 <- randBytes 10
    k2 <- randBytes 10
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
    let zeros = L.toStrict . D.toLazyByteString $ zeroBuilder in
    let (val, z) = BS.splitAt 80 check in
    if z == zeros
        then Just check
        else Nothing
    

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
module Utils where
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Data.Bits
import qualified Data.ByteString                as BS

import           Data.Int
import           Data.Word
import           System.Entropy
import           Text.Bytedump
import           Types                          hiding (not, (&&), (||))

import           Network.Socket
import qualified Network.Socket.ByteString.Lazy as LSBS

import           Pipes
import           Pipes.Lift

processOutputs ::  Socket -> [KeyContext] -> [SecureGraphBuilder] -> (SecureGraphBuilder -> GenM Bool) -> IO [Bool]
processOutputs soc keys values wrapOutputs = do
  let toBool = map wrapOutputs values
  let doReaders = map (runReaderP keys) toBool
  let attachServer = map (\x -> (lift $ LSBS.recv soc (fromIntegral cipherSize)) >~ for x (lift . LSBS.sendAll soc)) doReaders
  mapM runEffect attachServer


--Generation Functions
bitZero :: SecureGraphBuilder
bitZero = wrapNode $ Constant False

bitOne :: SecureGraphBuilder
bitOne = wrapNode $ Constant True

secureNumber :: Int -> [SecureGraphBuilder]
secureNumber n = map (const gInput) [1 .. n]

--Parameters
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

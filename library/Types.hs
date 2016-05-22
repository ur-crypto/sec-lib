{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where
import           Control.Monad.Trans.Reader
import           Crypto.Cipher.AES
import           Data.ByteString            as BS
import           Data.ByteString.Lazy       as LBS
import           Network.Socket
import           Pipes
import           Text.Bytedump

type Key = BS.ByteString
type CTT = TruthTable Key
type PTT = TruthTable (Key, Key, Key)
type TruthTable a = [a,a,a,a]
type GenM = Pipe LBS.ByteString LBS.ByteString (ReaderT [KeyContext] IO)
type SecureNumM = GenM SecureNum
type KeyM = GenM KeyType
data Literal = Constant Bool
              | Input {keym :: KeyM}
type SecureGate = Literal -> Literal -> Literal
type SecureNum = [Literal]
type SecureFunction = SecureNum -> SecureNum -> SecureNum

type FixedKey = AES128

type GenKey a = (Socket, [KeyContext], IO a)

data KeyType = Consumer !Key
             | Producer !Key !Key
             | Counter {andCount :: !Int, orCount :: !Int, xorCount :: !Int, notCount :: !Int} deriving (Eq)

instance Show KeyType where
  show (Consumer a) = dumpBS a
  show (Producer a b) = dumpBS a ++ "\n" ++ dumpBS b
  show (Counter a o x n) = "AND: " ++ show a ++ "\tOR: " ++ show o ++ "\tXOR: " ++ show x ++ "\tNOT: " ++  show n

data KeyContext = AES FixedKey
                | RAND Key

data GateType = AND | OR | XOR deriving (Show)

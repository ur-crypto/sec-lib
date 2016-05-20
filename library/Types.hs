{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where
import           Control.Monad.Memo
import           Crypto.Cipher.AES
import           Data.ByteString    as BS
import           Data.Map
import           Network.Socket
import           Text.Bytedump

type Key = BS.ByteString
type CTT = TruthTable Key
type PTT = TruthTable (Key, Key, Key)
type TruthTable a = [a,a,a,a]
data Literal = Constant Bool
              | Input {soc :: Socket, keys ::  [KeyContext], value :: IO (KeyType, Map (KeyType, KeyType) KeyType) }
type SecureGate = Literal -> Literal -> Literal
type SecureNum = [Literal]
type SecureFunction = SecureNum -> SecureNum -> SecureNum

type FixedKey = AES128

type GenKey a = (Socket, [KeyContext], IO a)

type GateMemo = (KeyType, KeyType) -> MemoT (KeyType, KeyType) KeyType IO KeyType

data KeyType = Consumer !Key
             | Producer !Key !Key
             | Counter {andCount :: !Int, orCount :: !Int, xorCount :: !Int, notCount :: !Int} deriving (Eq, Ord)

instance Show KeyType where
  show (Consumer a) = dumpBS a
  show (Producer a b) = dumpBS a ++ "\n" ++ dumpBS b
  show (Counter a o x n) = "AND: " ++ show a ++ "\tOR: " ++ show o ++ "\tXOR: " ++ show x ++ "\tNOT: " ++  show n

data KeyContext = AES FixedKey
                | RAND Key

data GateType = AND | OR | XOR deriving (Show)

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where
import           Control.Monad.State.Lazy
import           Crypto.Cipher.AES
import           Data.ByteString          as BS
import           Data.ByteString.Lazy     as LBS
import           Text.Bytedump

type Key = BS.ByteString
type CTT = TruthTable Key
type PTT = TruthTable (Key, Key, Key)
type TruthTable a = [a,a,a,a]
type KeyState = State LBS.ByteString KeyType
data Literal = Constant Bool
              | Input {keys :: [KeyContext], keyState :: KeyState}
type SecureGate = Literal -> Literal -> Literal
type SecureNum = [Literal]
type SecureFunction = SecureNum -> SecureNum -> SecureNum

type FixedKey = AES128

data KeyType = Consumer Key
             | Producer Key Key
             | Counter {andCount :: !Int, orCount :: !Int, xorCount :: !Int, notCount :: !Int} deriving (Eq)

instance Show KeyType where
  show (Consumer a) = dumpBS a
  show (Producer a b) = dumpBS a ++ "\n" ++ dumpBS b
  show (Counter a o x n) = "AND: " ++ show a ++ "\tOR: " ++ show o ++ "\tXOR: " ++ show x ++ "\tNOT: " ++  show n

data KeyContext = AES FixedKey
                | RAND Key

data GateType = AND | OR | XOR deriving (Show)

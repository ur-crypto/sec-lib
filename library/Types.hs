{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where
import           Crypto.Cipher.AES
import           Data.ByteString   as BS
import           Network.Socket

type Key = BS.ByteString
type CTT = TruthTable Key
type PTT = TruthTable (Key, Key, Key)
type TruthTable a = [a,a,a,a]
data Literal = Constant Bool
               | Input (GenKey KeyType)
type SecureGate = Literal -> Literal -> Literal
type SecureNum = [Literal]
type SecureFunction = SecureNum -> SecureNum -> SecureNum

type FixedKey = AES128

type GenKey a = (Socket, [KeyContext], IO a)

data KeyType = Consumer  Key
             | Producer (Key, Key) deriving (Eq)

data KeyContext = AES FixedKey
                | RAND Key

data GateType = AND | OR | XOR deriving (Show)

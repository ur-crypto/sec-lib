{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where
import           Control.Monad.RWS.Strict
import           Crypto.Cipher.AES
import           Data.ByteString          as BS
import           Data.LargeWord
import           Data.Map.Strict
import           Data.Vector.Sized

type Key = Word128

data PKey = PKey Key Key
data CKey = CKey Key

newtype FixedKey = FixedKey AES128
newtype RandKey = RandKey Key

data Counter = Counter {andCount :: !Int, orCount :: !Int, xorCount :: !Int, notCount :: !Int}

data KeyCache a = KeyCache (Map (SecureBit a) a)

data SecureBit a where
  Not :: SecureBit a -> SecureBit a
  And :: SecureBit a -> SecureBit a -> SecureBit a
  Or :: SecureBit a -> SecureBit a -> SecureBit a
  Xor :: SecureBit a -> SecureBit a -> SecureBit a
  SecureConstant :: Bool -> SecureBit b
  SecureProducer :: ((RWST (FixedKey, BS.ByteString) () (KeyCache PKey) IO) PKey) -> SecureBit PKey
  SecureConsumer :: ((RWST (FixedKey, BS.ByteString) () (KeyCache CKey) IO) CKey) -> SecureBit CKey
  SecureCounter :: Counter -> SecureBit Counter

type SecureNum a n = Vector (SecureBit a) n

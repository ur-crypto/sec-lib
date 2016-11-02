{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where
import           Control.Monad.RWS.Strict
import           Control.Monad.State.Strict
import           Crypto.Cipher.AES
import           Data.ByteString            as BS
import           Data.LargeWord
import           Data.Map.Strict
import GHC.TypeLits

type RealKey = Word128

type KeyId = Int

data Key a where
  PKey :: (RealKey, RealKey) -> Key (RealKey, RealKey)
  CKey :: RealKey -> Key RealKey
  BKey :: Bool -> Key a

newtype FixedKey = FixedKey {fKeyAES :: AES128}
newtype RandKey = RandKey {randKey :: RealKey}

data KeyMakerContext a where
  ConsumerContext :: FixedKey -> RandKey -> KeyMakerContext (RealKey, RealKey)
  ProducerContext :: RandKey -> KeyMakerContext RealKey

data SecureGate a n where
  Not :: Key a -> SecureGate a 1
  And :: Key a -> Key a -> SecureGate a 2
  Or :: Key a -> Key a -> SecureGate a 2
  Xor :: Key a -> Key a -> SecureGate a 2

type SecureBit a = (StateT (Map KeyId (Key a)) IO (Key a))

type SecureNum a = [SecureBit a]

class KeyMaker a where
  make :: KeyMakerContext a -> SecureGate a n -> ByteString -> (Key a, ByteString)

instance KeyMaker (RealKey, RealKey) where
  make context gate input = _
    where
      ConsumerContext FixedKey{fKeyAES} RandKey{randKey} = context

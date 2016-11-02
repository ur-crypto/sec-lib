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
import           Data.Type.Natural
import           Data.Vector.Sized

type RealKey = Word128

type KeyId = Int

data Key a where
  PKey :: (RealKey, RealKey) -> Key (RealKey, RealKey)
  CKey :: RealKey -> Key RealKey
  BKey :: Bool -> Key a

newtype FixedKey = FixedKey {fKeyAES :: AES128}
newtype RandKey = RandKey {randKey :: RealKey}

data KeyMakerContext a where
  ProducerContext :: FixedKey -> RandKey -> KeyMakerContext (RealKey, RealKey)
  ConsumerContext :: RandKey -> KeyMakerContext RealKey

data GateType args where
  Not :: GateType 1
  And :: GateType 2
  Or :: GateType 2
  Xor :: GateType 2

newtype SecureGate a n = SecureGate {gateType :: GateType n, args :: Vector a n}

type SecureBit a = (StateT (Map KeyId (Key a)) IO (Key a))

type SecureNum a = [SecureBit a]

class KeyMaker a where
  make :: KeyMakerContext a -> SecureGate a n -> ByteString -> (Key a, ByteString)

instance KeyMaker (RealKey, RealKey) where
  make context gate input = _
    where
      ProducerContext FixedKey{fKeyAES} RandKey{randKey} = context

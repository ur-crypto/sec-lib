{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where
import           Crypto.Cipher.AES
import           Data.LargeWord

type RealKey = Word128

type KeyId = Int

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



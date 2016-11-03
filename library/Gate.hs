{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Gate where
import           Control.Monad.State.Strict
import           Data.Binary
import           Data.Bits
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.List
import           Data.Map.Strict            (Map)
import           Data.Vector.Sized          (Vector)
import qualified Data.Vector.Sized          as V
import           Types
import           Utils

data SecureGate a n = SecureGate {gateType :: GateType n, args :: Vector n a}

data Key a where
  PKey :: (RealKey, RealKey) -> Key (RealKey, RealKey)
  CKey :: RealKey -> Key RealKey
  BKey :: Bool -> Key a

type SecureBit a = (StateT (Map KeyId (Key a)) IO (Key a))

type SecureNum a n = Vector n (SecureBit a)

class KeyMaker a where
  make :: KeyMakerContext a -> SecureGate a n -> ByteString -> (Key a, ByteString)

instance KeyMaker (RealKey, RealKey) where
  make context gate _ =
    case gate of
      SecureGate Not vec ->
        let (a, b) = V.index vec 0 in (PKey (b, a), BS.empty)
      SecureGate Xor vec ->
        let [(a0, _), (b0, b1)] = V.toList vec
            o0 = xor a0 b0
            o1 = xor a0 b1 in
          (PKey (o0, o1), BS.empty)
      SecureGate And vec ->
        let [(a0, a1), (b0, b1)] = V.toList vec in
          doNetwork $ [(a0, b0, False), (a0, b1, False), (a1, b0, False), (a1, b1, True)]
      SecureGate Or vec ->
        let [(a0, a1), (b0, b1)] = V.toList vec in
          doNetwork $ [(a0, b0, False), (a0, b1, True), (a1, b0, True), (a1, b1, True)]
    where
      ProducerContext FixedKey{fKeyAES} RandKey{randKey} = context
      doNetwork unsorted =
        let sorted = sortBy order unsorted
            o@(o0, o1) = mkKeyPair fKeyAES randKey sorted
            tt = map (insertKey o) sorted
            list = map (enc fKeyAES) tt
            joinedlist = mconcat $ map (decode . encode) list :: ByteString in
          (PKey (o0, o1), joinedlist)
      order (z1, z2, _) (z3, z4, _) =
        let p' = testBit z1 0
            q' = testBit z2 0
            r' = testBit z3 0
            s' = testBit z4 0 in
            case (compare p' r', compare q' s') of
              (EQ, EQ) -> error "List should not contain same element"
              (EQ, x) -> x
              (x, _) -> x
      mkKeyPair fkey rkey ((x, y, oB):_) =
        let
          k' = hashPair fkey x y
          in mkRealKeyPairFromRealKey rkey k' oB
      mkKeyPair _ _ _ = error "Attempting to make key pair of empty list"
      insertKey (o0, o1) (x, y, bool) = if bool then (x, y, o1) else (x, y, o0)

instance KeyMaker RealKey where
  make context gate input =
    case gate of
      SecureGate Not vec -> (CKey (V.index vec 0), BS.empty)
      SecureGate Xor vec -> 

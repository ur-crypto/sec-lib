{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Ops where
import           Data.Singletons
import           Data.Singletons.Prelude.Num
import           Data.Type.Natural           (Nat (..))
import qualified Data.Vector.Sized           as S
import           Prelude                     hiding (not, (&&), (/=), (==),
                                              (||))
import qualified Prelude                     as P
import           Types

--Gate Macros

class Boolean a where
  not :: a -> a
  (&&) :: a -> a -> a
  (||) :: a -> a -> a

  (/=) :: a -> a -> a
  (/=) a b = (not a && b) || (a && not b)
  xor :: a -> a -> a
  xor = (/=)
  (==) :: a -> a -> a
  a == b = not (a /= b)
  if' :: a -> a -> a -> a
  if' a b c = (a && b) || (not a && c)

instance Boolean P.Bool where
  not = P.not
  (&&) = (P.&&)
  (||) = (P.||)
  (==) = (P.==)
  (/=) = (P./=)

instance Boolean (SecureBit a) where
  not = Not
  (&&) = And
  (||) = Or
  (/=) = Xor

extend :: forall a n m. SingI m => SecureBit a -> SecureNum a n -> SecureNum a (m :+ n)
extend signBit vec = S.append vec (S.replicate' signBit)

instance Boolean (SecureNum a ('S n)) where
  not :: (SecureNum a ('S n)) -> SecureNum a ('S n)
  not vec =
    S.append (S.singleton $ not (S.foldl1 (||) vec)) $
    S.map (const $ SecureConstant False) $ S.tail vec
  (&&) a b = _

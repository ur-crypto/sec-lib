{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Ops where
import           Data.Type.Natural           (Nat (..))
import qualified Data.Vector.Sized           as S
import           Prelude                     hiding (not, (&&), (/=), (==),
                                              (||))
import qualified Prelude                     as P
import           Types

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

instance Boolean (SecureNum a ('S n)) where
  not :: (SecureNum a ('S n)) -> SecureNum a ('S n)
  not vec =
    S.append (S.singleton $ not (S.foldl1 (||) vec)) $
    S.map (const $ SecureConstant False) $ S.tail vec
  (&&) a b = S.append (S.singleton $ (S.head $ not $ not a) && (S.head $ not $ not b)) (S.tail a)
  (||) a b = S.append (S.singleton $ (S.head $ not $ not a) || (S.head $ not $ not b)) (S.tail a)

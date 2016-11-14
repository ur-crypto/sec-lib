{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Ops where
import           Prelude hiding (not, (&&), (/=), (==), (||))
import qualified Prelude as P
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

-- instance Boolean (SecureBit a) where
--   not = evalGate . Not
--   a && b = evalGate $ And a b
--   a || b = evalGate $ Or a b
--   a /= b = evalGate $ Xor a b

-- wrapBool :: forall a. Bool -> SecureBit a
-- wrapBool = return . BKey

-- bitsToLength :: forall a m n. SecureNum a n -> SecureNum a m
-- bitsToLength vec = generate vecMaker
--   where
--     vecMaker i = if i < length vec then index vec i else const $ wrapBool False


-- instance Boolean (SecureNum a) where
--   not list = extendToLength (length list) [foldl1 (||) list]
--   (&&) a b = zipWith (&&) (not $ not a) (not $ not b)
--   (||) a b = zipWith (||) (not $ not a) (not $ not b)


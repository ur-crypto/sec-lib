{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types where
import           Crypto.Cipher.AES
import           Data.ByteString   as BS
import           Network.Socket

type Key = BS.ByteString
type CTT = TruthTable Key
type PTT = TruthTable (Key, Key, Key)
type TruthTable a = [a,a,a,a]

type FixedKey = AES128
type SecureGate v = Literal v -> Literal v -> Literal v
type SecureFunction a = SecureNum a -> SecureNum a -> SecureNum a
type SecureNum a = [Literal a]
type FullKey a = (Socket, [KeyContext], IO a)
data Literal a  = Constant Bool
                | Input (FullKey a)
data KeyContext = AES FixedKey
                | RAND Key

data GateType = AND | OR | XOR

constants :: GateType -> Bool -> Bool -> Bool
constants AND a b = (&&) a b
constants OR a b = (||) a b
constants XOR a b = if a then not b else b



partialConstant :: GateType -> Literal v -> Bool -> Literal v
partialConstant AND _ False = Constant False
partialConstant AND key True = key
partialConstant OR key False = key
partialConstant OR _ True = Constant True
partialConstant XOR key False = key
partialConstant XOR key True = notGate key
checkConstant :: GateType -> Literal v -> Literal v -> Maybe (Literal v)
checkConstant ty (Constant a) (Constant b) = Just $ Constant $ constants ty a b
checkConstant ty (Constant a) (Input b) = Just $ partialConstant ty (Input b) a
checkConstant ty (Input a) (Constant b) = Just $ partialConstant ty (Input a) b
checkConstant _ (Input _) (Input _) = Nothing
class Circuitable v where
  defaultGate :: SecureGate v
  andGate :: SecureGate v
  andGate = defaultGate
  orGate :: SecureGate v
  orGate = defaultGate
  xorGate :: SecureGate v
  xorGate = defaultGate
  notGate :: Literal v -> Literal v

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types
  ( module Types
  , module SecureGraphs
  ) where

import           Control.Monad.Trans.Reader
import           Crypto.Cipher.AES
import           Data.ByteString            as BS
import           Network.Socket
import           Pipes
import           Text.Bytedump

import           SecureGraphs

type Key = BS.ByteString
type SecurePipe = Pipe KeyType KeyType (ReaderT ([KeyContext]) IO) ()

type FixedKey = AES128

type GenKey a = (Socket, [KeyContext], IO a)

data KeyType = Consumer Key
             | Producer Key Key
             | Constant Bool

instance Show KeyType where
  show (Consumer a) = dumpBS a
  show (Producer a b) = dumpBS a ++ "\n" ++ dumpBS b
  show (Constant a) = show a

data KeyContext = AES FixedKey
                | RAND Key


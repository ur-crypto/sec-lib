{-# LANGUAGE NamedFieldPuns #-}
module NotGate where
import           Pipes
import           Types

notGate :: Literal -> Literal
notGate (Constant b) = Constant $ not b
notGate (Input calculation) =
  Input (for calculation changed)
  where
    changed a' =
      yield $ case a' of
        Producer a0 a1 x -> Producer a1 a0 x
        Consumer x -> Consumer x
        rec@Counter {notCount = n} -> rec{notCount = n + 1}

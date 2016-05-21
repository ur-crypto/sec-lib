{-# LANGUAGE NamedFieldPuns #-}
module NotGate where
import           Types

notGate :: Literal -> Literal
notGate (Constant b) = Constant $ not b
notGate Input {keys, keyState} =
  Input keys changed
  where
    changed =
      case keyState of
        Producer a0 a1 ab -> Producer a1 a0 ab
        s@Consumer{} -> s
        rec@Counter {notCount = n} -> rec{notCount = n + 1}

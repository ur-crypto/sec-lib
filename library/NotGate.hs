{-# LANGUAGE NamedFieldPuns #-}
module NotGate where
import           Types

notGate :: Literal -> Literal
notGate (Constant b) = Constant $ not b
notGate Input {keys, keyState} =
  Input keys changed
  where
    changed = do
      a' <- keyState
      case a' of
        Producer a0 a1 -> return $ Producer a1 a0
        Consumer x -> return $ Consumer x
        rec@Counter {notCount = n} -> return $ rec{notCount = n + 1}

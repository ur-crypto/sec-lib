{-# LANGUAGE NamedFieldPuns #-}
module NotGate where
import           Types

notGate :: Literal -> Literal
notGate (Constant b) = Constant $ not b
notGate Input {soc, keys, value} =
  Input soc keys changed
  where
    changed = do
      (a', mCache) <- value
      case a' of
        Producer a0 a1 -> return (Producer a1 a0, mCache)
        Consumer x -> return (Consumer x, mCache)
        rec@Counter {notCount = n} -> return (rec{notCount = n + 1}, mCache)

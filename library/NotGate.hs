module NotGate where
import           Types

notGate :: Literal -> Literal
notGate (Constant b) = Constant $ not b
notGate (Input (soc, keys, a)) =
  Input (soc, keys, changed)
  where
    changed = do
      a' <- a
      case a' of
        Producer (a0, a1) -> return $ Producer (a1, a0)
        Consumer x -> return $ Consumer x

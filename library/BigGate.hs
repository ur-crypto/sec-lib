{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
module BigGate where
import           Control.Monad.State.Lazy
import           Control.Parallel.Strategies
import           Data.Bits
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import           Data.List
import           Debug.Trace
import           NotGate
import           Types
import           Utils

bigGate :: GateType -> SecureGate
bigGate ty (Constant a) (Constant b) =
  Constant $ case ty of
    AND -> a && b
    OR -> a || b
    XOR -> if a then not b else b
bigGate ty b@(Constant _) a@Input {} = bigGate ty a b
bigGate ty (a@Input {}) (Constant b) = partialConstant ty a b
  where
    partialConstant AND _ False = Constant False
    partialConstant AND key True = key
    partialConstant OR key False = key
    partialConstant OR _ True = Constant True
    partialConstant XOR key False = key
    partialConstant XOR key True = notGate key
bigGate ty Input {keys = fkeys, keyState = a} Input { keyState = b} =
  Input fkeys val
  where
    val = do
      curState <- get
      let (a', i) = runState a curState
      let (b', upState) = runState b i
      put $ trace (show a' ++ "\n" ++ show b') upState
      case (a', b') of
        (Producer x0 x1, Producer y0 y1) -> doProducer (x0, x1) (y0, y1)
        (Consumer x, Consumer y) -> doConsumer x y
        (x@Counter {}, y@Counter {}) -> doCount x y
        _ -> error "Should not combine types"
      where
        doCount p q =
          let merge = Counter {andCount = andCount p + andCount q, orCount = orCount p + orCount q, xorCount = xorCount p + xorCount q, notCount = notCount p + notCount q} in
          return $ case ty of
            AND -> merge {andCount = andCount merge + 1}
            OR -> merge {orCount = orCount merge + 1}
            XOR -> merge {xorCount = xorCount merge + 1}
        doConsumer :: BS.ByteString -> BS.ByteString -> KeyState
        doConsumer p q =
          case ty of
            XOR ->
              return $ Consumer (BS.pack $ BS.zipWith xor p q)
            _ -> do
              let [AES fkey] = fkeys
              byteTT <- get
              let (truthTable, rest) = getInput byteTT
              put $ trace (foldl (\w x -> (w ++ "\n" ++ x)) "" $ map keyString truthTable) rest
              let decrypt = decTruthTable fkey p q truthTable
              return $ Consumer $ trace ("\n" ++ keyString decrypt ++ "\n") decrypt
              where
                decTruthTable fkey k1 k2 [o00, o01, o10, o11] =
                  let k1' = testBit (BS.last k1) 0
                      k2' = testBit (BS.last k2) 0 in
                  let o = case (k1', k2') of
                        (False, False) -> o00
                        (False, True) -> o01
                        (True, False) -> o10
                        (True, True) -> o11
                        in
                  enc fkey (k1, k2, o)
                getInput tt =
                  let (x1, r1) = LBS.splitAt cipherSize tt
                      (x2, r2) = LBS.splitAt cipherSize r1
                      (x3, r3) = LBS.splitAt cipherSize r2
                      (x4, r4) = LBS.splitAt cipherSize r3
                      in
                  let tt' = map LBS.toStrict [x1, x2, x3, x4] in
                      (tt', r4)

        doProducer :: (Key, Key) -> (Key, Key) -> KeyState
        doProducer p q =
          case ty of
            XOR -> do
              let (a0, _) = p
                  (b0, b1) = q
              let o1 = BS.pack $ BS.zipWith xor a0 b0
                  o2 = BS.pack $ BS.zipWith xor a0 b1 in
                  return $! Producer o1 o2
            _ -> do
              -- putStrLn "New Gate"
              let [AES fkey, RAND rkey] = fkeys
              let unsorted = getTT ty (False, True) p q
              let sorted = sortBy order unsorted
              let o@(o0, o1) = mkKeyPair fkey rkey sorted
              let tt = map (insertKey o) sorted
              let list = parMap rdeepseq (enc fkey) tt
              let lazyList = map LBS.fromStrict (trace (foldl (\w x -> (w ++ "\n" ++ x)) "" $ map keyString list) list)
              let joinedList = mconcat lazyList
              curTable <- get
              put $ LBS.append curTable joinedList
              return $ trace ("\n" ++ keyString o0 ++ "\n" ++ keyString o1 ++ "\n") Producer o0 o1
              where
                  getTT AND (o0, o1) = helper o0 o0 o0 o1
                  getTT OR (o0, o1) = helper o0 o1 o1 o1
                  getTT XOR (o0, o1) = helper o0 o1 o1 o0
                  helper o1 o2 o3 o4 (a0, a1) (b0, b1)=
                    [(a0, b0, o1), (a0, b1, o2), (a1, b0, o3), (a1, b1, o4)]
                  order (z1, z2, _) (z3, z4, _) =
                    let p' = testBit (BS.last z1) 0
                        q' = testBit (BS.last z2) 0
                        r' = testBit (BS.last z3) 0
                        s' = testBit (BS.last z4) 0 in
                        case (compare p' r', compare q' s') of
                          (EQ, EQ) -> error "List should not contain same element"
                          (EQ, x) -> x
                          (x, _) -> x
                  mkKeyPair fkey rkey ((x, y, oB):_) =
                    let
                      k' = hashPair fkey x y
                      in mkKeyPairFromKey rkey k' oB
                  mkKeyPair _ _ _ = error "Attempting to make key pair of empty list"
                  insertKey (o0, o1) (x, y, bool) = if bool then (x, y, o1) else (x, y, o0)

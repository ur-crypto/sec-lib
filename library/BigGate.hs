{-# LANGUAGE BangPatterns #-}
module BigGate where
import           Control.Parallel.Strategies
import           Data.Bits
import qualified Data.ByteString             as BS
import           Data.List
import qualified Network.Socket.ByteString   as SBS
import           NotGate
import           Types
import           Utils

bigGate :: GateType -> SecureGate
bigGate ty (Constant a) (Constant b) =
  Constant $ case ty of
    AND -> a && b
    OR -> a || b
    XOR -> if a then not b else b
bigGate ty (Constant b) (Input a) = bigGate ty (Input a) (Constant b)
bigGate ty (Input a) (Constant b) = partialConstant ty (Input a) b
  where
    partialConstant AND _ False = Constant False
    partialConstant AND key True = key
    partialConstant OR key False = key
    partialConstant OR _ True = Constant True
    partialConstant XOR key False = key
    partialConstant XOR key True = notGate key
bigGate ty (Input (soc, fkeys, !a)) (Input (_,_,!b)) =
  Input (soc, fkeys, val)
  where
    val = do
      !a' <- a
      !b' <- b
      case (a', b') of
        (Producer x, Producer y) -> doProducer x y
        (Consumer x, Consumer y) -> doConsumer x y
        (Consumer _, Producer _) -> error "Should not combine types"
        (Producer _, Consumer _) -> error "Should not combine types"
      where
        doConsumer p q =
          case ty of
            XOR ->
              return $ Consumer (BS.pack $ BS.zipWith xor p q)
            _ -> do
              let [AES fkey] = fkeys
              -- putStrLn "New Gate"
              -- printKey (Just False) p
              -- printKey (Just False) q
              -- putStrLn ""
              ans <- getInput fkey p q
              -- printKey (Just False) ans
              -- putStrLn ""
              return $ Consumer ans
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
                getInput fkey x y = do
                  tt <- getTT
                  -- mapM_ (printKey (Just False)) tt
                  -- putStrLn ""
                  let !o = decTruthTable fkey x y tt
                  return o
                  where
                      getTT = do
                          byteTT <- SBS.recv soc (4 * cipherSize)
                          let (x1, r1) = BS.splitAt cipherSize byteTT
                          let (x2, r2) = BS.splitAt cipherSize r1
                          let (x3, x4) = BS.splitAt cipherSize r2
                          return [x1, x2, x3, x4]
        doProducer p q = do
          case ty of
            XOR -> do
              let (a0, _) = p
                  (b0, b1) = q
              let o1 = BS.pack $ BS.zipWith xor a0 b0
                  o2 = BS.pack $ BS.zipWith xor a0 b1 in
                  return $ Producer (o1, o2)
            _ -> do
              -- putStrLn "New Gate"
              let [AES fkey, RAND rkey] = fkeys
              let unsorted = getTT ty (False, True) p q
              let sorted = sortBy order unsorted
              -- let (p0, p1) = p
              -- let (q0, q1) = q
              -- printKey (Just True) p0
              -- printKey (Just True) p1
              -- printKey (Just True) q0
              -- printKey (Just True) q1
              -- putStrLn ""
              let o = mkKeyPair fkey rkey sorted
              let tt = map (insertKey o) sorted
              sendInfo fkey tt
              -- let (o0, o1) = o
              -- printKey (Just True) o0
              -- printKey (Just True) o1
              -- putStrLn ""
              return $ Producer o
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
                  sendInfo fkey tt = do
                    let list = encTruthTable tt
                    -- mapM_ (printKey (Just True)) list
                    -- putStrLn ""
                    SBS.sendMany soc list
                    where
                      encTruthTable = parMap rdeepseq (enc fkey)

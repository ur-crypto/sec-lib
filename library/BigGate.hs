{-# LANGUAGE BangPatterns #-}
module BigGate where
import           Data.Bits
import qualified Data.ByteString           as BS
import           Data.List
import qualified Network.Socket.ByteString as SBS
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
bigGate ty (Input (soc, fkeys, a)) (Input (_,_,b)) =
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
              ans <- getInput fkey p q
              return $ Consumer ans
              where
                decTruthTable fkey k1 k2 [o00, o01, o10, o11] =
                  let k1' = BS.last k1
                      k2' = BS.last k2 in
                  let o = case (k1', k2') of
                        (0, 0) -> o00
                        (0, 1) -> o01
                        (1, 0) -> o10
                        (1, 1) -> o11
                        _ -> error $ "Improper decoding of: " ++ show k1' ++ ", " ++ show k2'
                        in
                  enc fkey (k1, k2, o)
                getInput fkey x y = do
                  tt <- getTT
                  -- mapM_ (printKey (Just False)) tt
                  let o = decTruthTable fkey x y tt
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
              let (a0, a1) = p
                  (b0, b1) = q
              let o1 = BS.pack $ BS.zipWith xor a0 b0
                  o2 = BS.pack $ BS.zipWith xor a0 b1 in
                  return $ Producer (o1, o2)
            _ -> do
              let [AES fkey, RAND rkey] = fkeys
              let unsorted = getTT ty (False, True) p q
              let sorted = sortBy order unsorted
              let o = mkKeyPair fkey rkey sorted
              let tt = map (insertKey o) sorted
              sendInfo fkey tt
              return $ Producer o
              where
                  getTT AND (o0, o1) = helper o0 o0 o0 o1
                  getTT OR (o0, o1) = helper o0 o1 o1 o1
                  getTT XOR (o0, o1) = helper o0 o1 o1 o0
                  helper o1 o2 o3 o4 (a0, a1) (b0, b1)=
                    [(a0, b0, o1), (a0, b1, o2), (a1, b0, o3), (a1, b1, o4)]
                  order (z1, z2, _) (z3, z4, _) =
                    let p' = fromEnum $ BS.last z1
                        q' = fromEnum $ BS.last z2
                        r' = fromEnum $ BS.last z3
                        s' = fromEnum $ BS.last z4 in
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
                    SBS.sendMany soc list
                    where
                      encTruthTable = map (enc fkey)

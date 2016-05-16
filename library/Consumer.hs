{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Consumer where
import           Data.Bits
import qualified Data.ByteString           as BS
import           Network.Socket
import qualified Network.Socket.ByteString as SBS
import           Types
import           Utils


type CKey = Key
instance LocalValue CKey where
  defaultGate a b =
    case checkConstant AND a b of
      Just x -> x
      Nothing ->
        let Input (soc, [AES fkey], a') = a
            Input (_, _, b') = b in
        Input (soc, [AES fkey], getInput soc fkey a' b')
        where
          getInput soc fkey a' b' = do
            x <- a'
            y <- b'
            tt <- getTT soc
            let o = decTruthTable fkey x y tt
            return o
            where
                getTT soc = do
                    byteTT <- SBS.recv soc (4 * cipherSize)
                    let (x1, r1) = BS.splitAt cipherSize byteTT
                    let (x2, r2) = BS.splitAt cipherSize r1
                    let (x3, x4) = BS.splitAt cipherSize r2
                    return [x1, x2, x3, x4]
decTruthTable :: FixedKey -> Key -> Key -> [Key] -> Key
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

-- instance LocalValue Key where
--     notHandler (Input a) = return $ Input a
--     notHandler (Constant l) = return $ Constant $ not l
--     notHandler _ = error "Should not recieve Gate"
--     gateHandler _ _ XOR a b = do
--         let o = BS.pack $ BS.zipWith xor a b
--         return $ Input o
--     gateHandler (Just soc) [AES fkey] ty x y = do
--         tt <- getTT
--         let o = decTruthTable fkey x y tt
--         return $ Input $ o
--         where
--             getTT = do
--                 byteTT <- SBS.recv soc (4 * cipherSize)
--                 let (x1, r1) = BS.splitAt cipherSize byteTT
--                 let (x2, r2) = BS.splitAt cipherSize r1
--                 let (x3, x4) = BS.splitAt cipherSize r2
--                 return $ TruthTable x1 x2 x3 x4
--     gateHandler Nothing _ _ _ _ = error "Needs a socket"
--     gateHandler _ _ _ _ _ = error "Needs Correct Keys"

getSocket :: IO Socket
getSocket = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "3000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress serveraddr)
    listen sock 1
    (conn, _) <- accept sock
    return conn

doWithSocket :: FiniteBits a => Socket -> (a, a) -> SecureFunction CKey-> IO [Bool]
doWithSocket soc (produceInput, consumeInput) test = do
    let l = finiteBitSize produceInput + finiteBitSize consumeInput
    fkeystr <- SBS.recv soc cipherSize
    let fkey = initFixedKey fkeystr
    keyList <- receiveList l
    let wrapVal soc fkey k= Input (soc, fkey, return k)
    let (theirList, ourList) = splitAt (finiteBitSize produceInput) $ map (wrapVal soc [AES fkey]) keyList
    receiveOutputs $ test theirList ourList
    where
      receiveList :: Int -> IO [Key]
      receiveList num = mapM (const $ SBS.recv soc cipherSize) [1..num]
      receiveOutputs :: [Literal Key] -> IO [Bool]
      receiveOutputs = mapM receiveNodes
          where
          receiveNodes :: Literal Key -> IO Bool
          receiveNodes (Input (_, _, k')) = do
              o0 <- SBS.recv soc cipherSize
              o1 <- SBS.recv soc cipherSize
              k <- k'
              SBS.sendAll soc k
              return $ if
                  | k == o0 -> False
                  | k == o1 -> True
                  | otherwise -> error $ "Incorrect answer found: " ++ show k ++ show o0 ++ show o1
          receiveNodes (Constant k) = return k
          -- receiveNodes (Not k) = receiveNodes k
          receiveNodes x = error "Should not be receiving gate"

doWithoutSocket ::FiniteBits a => (a, a) -> SecureFunction CKey -> IO [Bool]
doWithoutSocket input test = do
    soc <- getSocket
    ans <- doWithSocket soc input test
    close soc
    return ans

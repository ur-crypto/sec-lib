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

doWithSocket :: FiniteBits a => Socket -> (a, a) -> SecureFunction -> IO [Bool]
doWithSocket soc (produceInput, consumeInput) test = do
    let l = finiteBitSize produceInput + finiteBitSize consumeInput
    fkeystr <- SBS.recv soc cipherSize
    -- putStr "Fkey"
    -- printKey (Just False) fkeystr
    let fkey = initFixedKey fkeystr
    keyList <- receiveList l
    -- putStrLn "Key List:"
    -- mapM_ (printKey (Just False)) keyList
    -- putStrLn ""
    let wrapVal k= Input soc [AES fkey] (return (Consumer k))
    let (theirList, ourList) = splitAt (finiteBitSize produceInput) $ map wrapVal keyList
    receiveOutputs $ test theirList ourList
    where
      receiveList :: Int -> IO [Key]
      receiveList num = mapM (const $ SBS.recv soc cipherSize) [1..num]
      receiveOutputs :: [Literal] -> IO [Bool]
      receiveOutputs = mapM receiveNodes
          where
          receiveNodes :: Literal -> IO Bool
          receiveNodes Input {value = k'} = do
              (Consumer k) <- k'
              o0 <- SBS.recv soc cipherSize
              o1 <- SBS.recv soc cipherSize
              SBS.sendAll soc k
              return $ if
                  | k == o0 -> False
                  | k == o1 -> True
                  | otherwise -> error $ "Incorrect answer found: " ++ keyString k ++ keyString o0 ++ keyString o1
          receiveNodes (Constant k) = return k

doWithoutSocket ::FiniteBits a => (a, a) -> SecureFunction -> IO [Bool]
doWithoutSocket input test = do
    soc <- getSocket
    ans <- doWithSocket soc input test
    close soc
    return ans

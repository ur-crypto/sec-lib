{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Consumer where
import           Data.Bits
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LBS
import           Debug.Trace
import           Network.Socket
import qualified Network.Socket.ByteString      as SBS
import qualified Network.Socket.ByteString.Lazy as LSBS
import           Types
import           Utils


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

-- doWithSocket :: FiniteBits a => Socket -> (a, a) -> SecureFunction -> IO [Bool]
-- doWithSocket soc (produceInput, consumeInput) test = do
--     let l = finiteBitSize produceInput + finiteBitSize consumeInput
--     where
--       wrapList aesKey string accum =
--         if LBS.empty == string
--           then accum
--           else
--             let (key, rest) = LBS.splitAt (fromIntegral cipherSize) string in
--             let lit = Input [AES aesKey] $ return $ Consumer (LBS.toStrict key) in
--             wrapList aesKey rest accum ++ [lit]
--       receiveOutputs _ [] accum = return accum
--       receiveOutputs lazyString (x:xs) accum = do
--             (rest, tf) <- receiveNodes lazyString x
--             receiveOutputs rest xs (accum ++ [tf])
--               where
--                 receiveNodes initString(Left (Consumer k)) = do
--                   let (lo0, i) = LBS.splitAt (fromIntegral cipherSize) initString
--                   let (lo1, rest) = LBS.splitAt (fromIntegral cipherSize) i
--                   let (o0, o1) = (LBS.toStrict lo0, LBS.toStrict lo1)
--                   SBS.sendAll soc k
--                   let tf = if | k == o0 -> False
--                               | k == o1 -> True
--                               | otherwise -> error $ "Incorrect answer found: " ++ keyString k ++ keyString o0 ++ keyString o1
--                   return (rest, tf)
--                 receiveNodes x (Right k) = return (x, k)

-- doWithoutSocket ::FiniteBits a => (a, a) -> SecureFunction -> IO [Bool]
-- doWithoutSocket input test = do
--     soc <- getSocket
--     ans <- doWithSocket soc input test
--     close soc
--     return ans

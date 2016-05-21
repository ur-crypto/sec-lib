{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Consumer where
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LBS
import           Debug.Trace
import           Network.Socket
import qualified Network.Socket.ByteString      as SBS
import qualified Network.Socket.ByteString.Lazy as LSBS
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

--not merging the gates properly
processOutputs :: LBS.ByteString -> [Literal] ->(LBS.ByteString, [Either Bool Key])
processOutputs startString input =
    foldl assembler (startString, []) input
    where
      assembler :: (LBS.ByteString, [Either Bool Key]) -> Literal -> (LBS.ByteString, [Either Bool Key])
      assembler (curString, accum) (Input _ Consumer {string, getKey = nextP}) =
        (nextString, accum ++ [Right key])
        where
          Right (nextString, _, key) = runGetOrFail nextP curString
      assembler (str, accum) (Constant b) = (str, accum ++ [Left b])
      outputP full = do
        putLazyByteString startString
        putLazyByteString $ runPut full

doWithSocket :: FiniteBits a => Socket -> (a, a) -> SecureFunction -> IO [Bool]
doWithSocket soc (produceInput, consumeInput) test = do
    let l = finiteBitSize produceInput + finiteBitSize consumeInput
    lazyReceiver <- LSBS.getContents soc
    let (fkeystr, lazyKeys) = LBS.splitAt cipherSize lazyReceiver
        fkey = initFixedKey (LBS.toStrict fkeystr)
        (keyList, truthTables) = LBS.splitAt (cipherSize * fromIntegral l) lazyKeys
    -- print $ mconcat [fkeystr, keyList]
    let wrappedList = wrapList fkey keyList truthTables []
    let (ourWrappedList, theirWrappedList) = splitAt (finiteBitSize produceInput) wrappedList
    let secureFunc = test ourWrappedList theirWrappedList
    let (finalLazyString, sendList) = processOutputs truthTables secureFunc
    receiveOutputs finalLazyString sendList []
    where
      wrapList :: FixedKey -> LBS.ByteString -> LBS.ByteString -> [Literal] -> [Literal]
      wrapList aesKey keyStr tableString accum =
        if LBS.empty == keyStr
          then accum
          else
            let (key, rest) = LBS.splitAt (fromIntegral cipherSize) keyStr in
            let lit = Input [AES aesKey] $ Consumer tableString (return $ LBS.toStrict key) in
            wrapList aesKey rest tableString accum ++ [lit]
      receiveOutputs :: LBS.ByteString -> [Either Bool Key] -> [Bool] -> IO [Bool]
      receiveOutputs _ [] accum = return accum
      receiveOutputs lazyString (x:xs) accum = do
            (rest, tf) <- receiveNodes lazyString x
            -- print tf
            receiveOutputs rest xs (accum ++ [tf])
              where
                receiveNodes :: LBS.ByteString -> Either Bool Key -> IO (LBS.ByteString, Bool)
                receiveNodes x' (Left k) = return (x', k)
                receiveNodes initString (Right k) = do
                    let (lo0, i) = LBS.splitAt (fromIntegral cipherSize) initString
                    let (lo1, rest) = LBS.splitAt (fromIntegral cipherSize) i
                    let (o0, o1) = (LBS.toStrict lo0, LBS.toStrict lo1)
                    -- printKey (Just False) o0
                    -- printKey (Just True) o1
                    -- printKey Nothing k
                    SBS.sendAll soc k
                    let tf = if | k == o0 -> False
                                | k == o1 -> True
                                | otherwise -> error $ "Incorrect answer found: " ++ keyString k ++ keyString o0 ++ keyString o1
                    return (rest, tf)

doWithoutSocket ::FiniteBits a => (a, a) -> SecureFunction -> IO [Bool]
doWithoutSocket input test = do
    soc <- getSocket
    ans <- doWithSocket soc input test
    close soc
    return ans

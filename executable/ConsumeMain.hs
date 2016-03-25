{-# LANGUAGE RebindableSyntax #-}
import Prelude hiding (ifThenElse)
import Consumer
import Network.Socket
import Network.Socket.ByteString as SBS
import Utils
import Types

getSocket :: IO Socket
getSocket = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "3000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 1
    (conn, _) <- accept sock
    return conn

main :: IO ()
main = do
    soc <- getSocket
    ka <- SBS.recv soc cipherSize
    kb <- SBS.recv soc cipherSize
    kc <- SBS.recv soc cipherSize
    putStrLn "Starting Keys (from OT)"
    printKey ka
    printKey kb
    printKey kc

    let (a, b, c) = (Input ka, Input kb, Input kc)
    let eq =  if c then a else b
    (Input o) <- processGate  soc eq
    putStrLn ""
    putStrLn "Answer"
    printKey o
    return ()

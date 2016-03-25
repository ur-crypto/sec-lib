{-# LANGUAGE RebindableSyntax #-}
import Prelude hiding (ifThenElse)
import Producer
import Network.Socket
import Network.Socket.ByteString as SBS
import Utils
import Types

getSocket :: IO Socket
getSocket = do
    addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "3000")
    let serveraddr = head addrinfos
    soc <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect soc (addrAddress serveraddr)
    return soc

main :: IO ()
main = do
    (ka0, ka1) <- genKeyPair
    (kb0, kb1) <- genKeyPair
    (kc0, kc1) <- genKeyPair
    let (a, b, c) = (Input (ka0, ka1), Input (kb0, kb1), Input (kc0, kc1))
    putStrLn "Start Keys"
    let (t, f) = (Just True, Just False)

    printKey f ka0 
    printKey t ka1
    printKey f kb0
    printKey t kb1
    printKey f kc0
    printKey t kc1

    soc <- getSocket
    SBS.sendAll soc ka0
    SBS.sendAll soc kb1
    SBS.sendAll soc kc0
    putStrLn ""
    let eq =  if c then a else b
    (Input (o1, o2)) <- processGate soc eq

    putStrLn "Answers"
    printKey f o1
    printKey t o2
    return ()

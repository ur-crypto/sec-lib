module Producer where
import Utils
import Types
import Network.Socket
import Network.Socket.ByteString

type PKey = Value (Key, Key)
type PTT = TruthTable (Key, Key, Key)

sendInfo :: Socket -> PTT -> IO()
sendInfo soc (TruthTable r1 r2 r3 r4) = do
    let outkeys = map encOutKey [r1, r2, r3, r4]
    sendMany soc outkeys

processGate :: Socket -> PKey -> IO PKey
processGate soc (Gate t k1 k2) = do
    a <- processGate soc k1
    b <- processGate soc k2
    ok <- genKeyPair
    let o = (Input ok)
    let tt = getTT t o a b
    sendInfo soc tt
    return o
    where
    helper o1 o2 o3 o4 (Input (a0, a1)) (Input (b0, b1))=
        TruthTable (a0, b0, o1) (a0, b1, o2) (a1, b0, o3) (a1, b1, o4)
    helper _ _ _ _ _ _ = error "Should only pass values"
    getTT AND (Input (o0, o1)) = helper o0 o0 o0 o1
    getTT OR (Input (o0, o1)) = helper o0 o1 o1 o1
    getTT XOR (Input (o0, o1)) = helper o0 o1 o1 o0
    getTT NAND (Input (o0, o1)) = helper o1 o1 o1 o0
    getTT BIJ (Input (o0, o1)) = helper o1 o0 o0 o1
    getTT _ _ = error "Should not pass gates to gates"

processGate _ (Input a) = return (Input a)


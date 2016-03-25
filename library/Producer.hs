module Producer where
import Utils
import Types
import Network.Socket
import Network.Socket.ByteString

type KeyPair = Value (Key, Key)

data TruthTable = TruthTable (Key, Key, Key) (Key, Key, Key)  (Key, Key, Key)  (Key, Key, Key) 
sendInfo :: Socket -> TruthTable -> IO()
sendInfo soc (TruthTable r1 r2 r3 r4) = do
    let outkeys = map encOutKey [r1, r2, r3, r4]
    sendMany soc outkeys

processGate :: Socket -> KeyPair -> IO KeyPair
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
    helper _ _ _ _ _ _ = error "Should not pass gate to truth table construct"
    getTT AND (Input (o0, o1)) = helper o0 o0 o0 o1
    getTT OR (Input (o0, o1)) = helper o0 o1 o1 o1
    getTT XOR (Input (o0, o1)) = helper o0 o1 o1 o0
    getTT NAND (Input (o0, o1)) = helper o1 o1 o1 o0
    getTT BIJ (Input (o0, o1)) = helper o1 o0 o0 o1
    getTT _ _ = error "Should not pass gates to gates"

processGate _ (Input a) = return (Input a)


--Circuit Macros
(.&.) :: KeyPair -> KeyPair -> KeyPair
(.&.) = Gate AND
(.|.) :: KeyPair -> KeyPair -> KeyPair
(.|.) = Gate OR
xor :: KeyPair -> KeyPair -> KeyPair
xor = Gate XOR 
nand :: KeyPair -> KeyPair -> KeyPair
nand = Gate NAND
bij :: KeyPair -> KeyPair -> KeyPair
bij = Gate BIJ

--If Then Else Macro
ifThenElse :: KeyPair -> KeyPair -> KeyPair -> KeyPair
ifThenElse bool tb fb = 
    let nbool = Gate NAND bool bool in
    ((bool .&. tb) .|. (nbool .&. fb))


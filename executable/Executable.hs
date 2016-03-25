import Producer
import Utils
import Types

main :: IO ()
main = do
    a <- genKeyPair
    b <- genKeyPair
    c <- genKeyPair
    let eq =  c .|. a .&. b
    _ <- processGate eq
    return ()

import Producer
import Utils

main :: IO ()
main = do
    (k1, k2) <- genKeyPair
    (k3, k4) <- genKeyPair
    (k5, k6) <- genKeyPair
    (o1, o2) <- return (k1, k2) .&. return (k3, k4)
    _ <- return (k5, k6) .|. return (o1, o2)
    return ()

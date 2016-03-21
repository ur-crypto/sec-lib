import Producer
import Utils
main :: IO ()
main = do
    k1 <- getKey
    k2 <- getKey
    k3 <- getKey
    k4 <- getKey
    _ <- return (k1, k2) .&. return (k3, k4)
    return ()

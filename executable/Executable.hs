import Producer
import Utils
main :: IO ()
main = do
    _ <- (getKey, getKey) .&. (getKey, getKey)
    return ()

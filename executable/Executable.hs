{-# RebindableSyntax #-}

import Producer
import Utils

main :: IO ()
main = do
    a <- genKeyPair
    b <- genKeyPair
    c <- genKeyPair
    _ <- ifThenElse (return a) (return b) (return c)
    return ()

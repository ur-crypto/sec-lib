{-# LANGUAGE RebindableSyntax #-}
import Prelude hiding (ifThenElse)
import Producer
import Utils
import Types

main :: IO ()
main = do
    a <- genKeyPair
    b <- genKeyPair
    c <- genKeyPair
    let eq =  if c then a else b
    _ <- processGate eq
    return ()

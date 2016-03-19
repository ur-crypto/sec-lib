module Crypto where


data KeyPair = KeyPair{key0::Int, key1::Int} deriving(Show)

plusTable :: KeyPair -> KeyPair -> KeyPair
plusTable (kp1) (kp2) = kp2

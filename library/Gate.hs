{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Gate where
import           BigGate
import           Data.Bits
import           NotGate
import           Types


-- type GateCounter = [Int]
andGate :: SecureGate
andGate = bigGate AND
orGate :: SecureGate
orGate = bigGate OR
xorGate :: SecureGate
xorGate = bigGate XOR


instance Eq Literal where
    (==) =  undefined
    (/=) =  undefined

instance Bits SecureNum where
    (.&.) = zipWith andGate
    (.|.) = zipWith orGate
    xor = zipWith xorGate
    complement = map notGate
    shiftL xs num =  drop num xs  ++  map ( const  $ Constant  False) [0 .. num  -1]
    shiftR xs num =  map ( const  $ Constant  False) [0 .. num  -1]  ++  take num xs
    rotate =  undefined
    -- rotate x st =  take ( length st)  $  drop ( negate x ` mod`  length st)  $  cycle st
    isSigned _ =  False
    testBit =  undefined
    bit _ =  undefined
    popCount =  undefined
    bitSize =  length
    bitSizeMaybe a =  Just $  length a

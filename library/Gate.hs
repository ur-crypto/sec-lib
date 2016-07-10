{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- # LANGUAGE FlexibleInstances    #
-- # LANGUAGE RankNTypes           #
-- # LANGUAGE TypeSynonymInstances #
module Gate where
import           Data.Bits
import           Types

import           Control.Monad.State.Strict

import           Data.Graph.Inductive.Graph

mergeGraph :: SecureGraph -> SecureGraph -> SecureGraph
mergeGraph graphA graphB =
  let bNodes = labNodes graphB
      bEdges = labEdges graphB in
    insEdges bEdges $ insNodes bNodes graphA

andGate :: SecureGate
andGate = bigGate AND
orGate :: SecureGate
orGate = bigGate OR
xorGate :: SecureGate
xorGate = bigGate XOR

bigGate :: GateType -> SecureGate
bigGate ty leftM rightM = do
  currentNumber <- get
  let (leftGraph, leftNum) = runState leftM currentNumber
      (rightGraph, rightNum) = runState rightM leftNum
      nextLeft = snd $ nodeRange leftGraph
      nextRight = snd $ nodeRange rightGraph
      newNode = nextRight + 1
  put newNode
  return $
    insEdge (nextLeft, newNode, ()) $
    insEdge (nextRight, newNode, ()) $
    insNode (newNode, Gate ty) $
    mergeGraph leftGraph rightGraph

notGate :: GraphBuilder -> GraphBuilder
notGate m = do
  currentNumber <- get
  let (graph, lastNumber) = runState m currentNumber 
  put $ currentNumber + 1
  return $
    insEdge(lastNumber, currentNumber + 1, ()) $
    insNode (currentNumber + 1 , Gate NOT) graph

instance Eq GraphBuilder where
  (==) = undefined
  (/=) = undefined

instance Bits SecureNum where
    (.&.) = zipWith andGate
    (.|.) = zipWith orGate
    xor = zipWith xorGate
    complement = map notGate
    -- shiftL xs num =  drop num xs  ++  map ( const $ Constant False) [0 .. num  -1]
    -- shiftR xs num =  map ( const  $ Constant  False) [0 .. num  -1]  ++  take num xs
    rotate =  undefined
    -- rotate x st =  take ( length st)  $  drop ( negate x ` mod`  length st)  $  cycle st
    isSigned _ =  False
    testBit =  undefined
    bit _ =  undefined
    popCount =  undefined
    bitSize =  length
    bitSizeMaybe a =  Just $  length a

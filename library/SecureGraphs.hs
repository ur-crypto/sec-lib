{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module SecureGraphs where

import           Prelude                           hiding (not, (&&), (||))

import qualified Data.Graph.Inductive.Graph        as G

import           Control.Monad.State.Strict
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree

import           Data.Bits

data GateType = AND | OR | XOR | NOT deriving (Show)
data SecureNode = Gate GateType | Input | Constant Bool deriving (Show)
type SecureGraph = Gr SecureNode ()
type SecureGraphBuilder = State Int SecureGraph
type SecureGate = SecureGraphBuilder -> SecureGraphBuilder -> SecureGraphBuilder
type SecureNum = [SecureGraphBuilder]
type SecureFunction = SecureNum -> SecureNum -> SecureNum

instance Monoid SecureGraph where
  mempty = G.empty
  mappend graphA graphB =
    let bNodes = labNodes graphB
        bEdges = labEdges graphB in
      insEdges bEdges $ insNodes bNodes graphA

instance Monoid SecureGraphBuilder where
  mempty = return G.empty
  mappend aGraphM bGraphM = do
    number <- get
    let (aGraph, aNum) = runState aGraphM number
        (bGraph, bNum) = runState bGraphM aNum
    put $ bNum + 1
    return $ mappend aGraph bGraph

wrapNode :: SecureNode ->  SecureGraphBuilder
wrapNode a = do
  number <- get
  put $ number + 1
  return $ G.insNode (number, a) G.empty

wrapConstant :: Bool -> SecureGraphBuilder
wrapConstant = wrapNode . Constant

gInput :: SecureGraphBuilder
gInput = wrapNode Input

bigGate :: GateType -> SecureGate
bigGate ty leftM rightM = do
  currentNumber <- get
  let (leftGraph, leftNum) = runState leftM currentNumber
      (rightGraph, rightNum) = runState rightM leftNum
      newNode = rightNum
  put $ newNode + 1
  return $
    insEdge (leftNum - 1 , newNode, ()) $
    insEdge (rightNum - 1 , newNode, ()) $
    insNode (newNode, Gate ty) $
    mappend leftGraph rightGraph

notGate :: SecureGraphBuilder -> SecureGraphBuilder
notGate m = do
  currentNumber <- get
  let (graph, lastNumber) = runState m currentNumber
  put $ lastNumber + 1
  return $
    insEdge(lastNumber - 1, lastNumber, ()) $
    insNode (lastNumber , Gate NOT) graph

buildGraph :: SecureGraphBuilder -> SecureGraph
buildGraph builder = evalState builder 0


andGate :: SecureGate
andGate = bigGate AND
orGate :: SecureGate
orGate = bigGate OR
xorGate :: SecureGate
xorGate = bigGate XOR

--Gate Macros

(&&) :: SecureGate
(&&) = andGate
(||) :: SecureGate
(||) = orGate
bXor :: SecureGate
bXor = xorGate
nand :: SecureGate
nand a b = notGate $ andGate a b
bij :: SecureGate
bij a b = notGate $ xorGate a b
not :: SecureGraphBuilder -> SecureGraphBuilder
not = notGate

instance Eq SecureGraphBuilder where
  (==) = undefined
  (/=) = undefined

instance Bits SecureNum where
    (.&.) = zipWith andGate
    (.|.) = zipWith orGate
    xor = zipWith xorGate
    complement = map notGate
    shiftL xs num =  drop num xs ++
      map ( const $ wrapNode $ Constant False) [0 .. num  -1]
    shiftR xs num =  map ( const $ wrapNode $ Constant  False)
      [0 .. num  -1]  ++  take num xs
    rotate =  undefined
    -- rotate x st =  take ( length st)  $  drop ( negate x ` mod`  length st)  $  cycle st
    isSigned _ =  False
    testBit =  undefined
    bit _ =  undefined
    popCount =  undefined
    bitSize =  length
    bitSizeMaybe a =  Just $  length a

instance Show SecureGraphBuilder where
  show = show . buildGraph

printFullGraph :: [SecureGraphBuilder] -> IO ()
printFullGraph = prettyPrint . buildGraph . mconcat 

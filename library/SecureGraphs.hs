{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}
module SecureGraphs (bigGate
                    , SecureGraph
                    , SecureGate
                    , createProgramCircuit
                    , SecureNum
                    , SecureFunction
                    , GateType(..)
                    , SecureGraphBuilder
                    , ComputableGraph(..)
                    , SecureNode(..)
                    , andGate
                    , orGate
                    , xorGate
                    , notGate
                    , wrapConstant
                    , module Data.Graph.Inductive.Query.DFS
                    ) where

import           Data.Bits
import           Data.List


import           Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Graph        as G
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.DFS

import           Control.Monad.State.Lazy

data GateType = AND | OR | XOR | NOT deriving (Show, Eq)
data SecureNode = Gate GateType | Input | Const Bool deriving (Show, Eq)
data ComputableGraph = ComputableGraph{graphNodes :: [Node], graph :: SecureGraph}
type SecureGraph = Gr SecureNode ()
data SecureGraphBuilder = Builder (State SecureGraph Int) | Constant Bool
type SecureGate = SecureGraphBuilder -> SecureGraphBuilder -> SecureGraphBuilder
type SecureNum = [SecureGraphBuilder]
type SecureFunction = SecureNum -> SecureNum -> SecureNum

isGate :: GateType -> SecureGraph -> Int -> Bool
isGate ty graph num =
  let mContext = fst $ match num graph in
    maybe False (\x -> Gate ty == lab' x) mContext

bigGate :: GateType -> SecureGate
bigGate ty (Constant a) (Constant b) =
  Constant $ case ty of
    AND -> a && b
    OR -> a || b
    XOR -> if a then not b else b
    NOT -> error "should never happen"
bigGate ty c'@(Constant _) b'@(Builder _) = bigGate ty b' c'
bigGate ty b'@(Builder _) c'@(Constant c) =
  case ty of
    AND -> if c then b' else c'
    OR -> if c then c' else b'
    XOR -> if c then notGate b' else b'
    NOT -> error "will never happen"
bigGate ty (Builder leftM) (Builder rightM) = Builder $ do
  enterGraph <- get
  let (leftTip, leftGraph) = runState leftM enterGraph
      (rightTip, currentGraph) = runState rightM leftGraph
      leftSucc = suc currentGraph leftTip
      rightSucc = suc currentGraph rightTip
      avail = head $ newNodes 1 currentGraph
      maybeNode = find (isGate ty currentGraph) $ union leftSucc rightSucc
  case maybeNode of
    Just x -> do
      put currentGraph
      return x
    Nothing -> do
      put $ insEdge (rightTip, avail, ()) $
        insEdge (leftTip, avail, ()) $
        insNode (avail, Gate ty) currentGraph
      return avail

notGate :: SecureGraphBuilder -> SecureGraphBuilder
notGate (Constant c) = Constant $ not c
notGate (Builder m) = Builder $ do
  enterGraph <- get
  let (currentTip, currentGraph) = runState m enterGraph
      tipGates = suc currentGraph currentTip
      maybeNode = find (isGate NOT currentGraph) tipGates
      avail = head $ newNodes 1 currentGraph
  case maybeNode of
    Just c -> do
      put currentGraph
      return c
    Nothing -> do
      put $ insEdge (currentTip, avail, ()) $
        insNode(avail, Gate NOT) currentGraph
      return avail

wrapConstant :: Bool -> SecureGraphBuilder
wrapConstant = Constant

generateInputs :: Int -> (SecureNum, SecureGraph)
generateInputs numberInputs = (inputs, inputsGraph)
  where
    inputs = map (Builder . return) [1 .. numberInputs]
    inputsGraph = foldr (\n g -> G.insNode (n, Input) g) G.empty [1 .. numberInputs]

createProgramCircuit :: SecureFunction -> Int -> Int -> ComputableGraph
createProgramCircuit func numberInputsA numberInputsB =
    let inputSize = numberInputsA + numberInputsB
        (startBuilders, startGraph) = generateInputs inputSize
        (aBuilders, bBuilders) = splitAt numberInputsA startBuilders
        builderResults = func aBuilders bBuilders in
      mergeResults (ComputableGraph [] startGraph) builderResults

mergeResults :: ComputableGraph -> SecureNum -> ComputableGraph
mergeResults graph [] = graph
mergeResults ComputableGraph{graphNodes, graph} (Builder result:results) =
  let (newNode, newGraph) = runState result graph in
  mergeResults (ComputableGraph (graphNodes ++ [newNode]) newGraph) results
mergeResults ComputableGraph{graphNodes, graph} (Constant result:results) =
  let [node] = newNodes 1 graph
      newGraph = G.insNode (node, Const result) graph in
    mergeResults ComputableGraph{graphNodes = graphNodes ++ [node], graph = newGraph} results

andGate :: SecureGate
andGate = bigGate AND
orGate :: SecureGate
orGate = bigGate OR
xorGate :: SecureGate
xorGate = bigGate XOR

--Gate Macros


instance Eq SecureGraphBuilder where
  (==) = undefined
  (/=) = undefined

instance Bits SecureNum where
    (.&.) = zipWith andGate
    (.|.) = zipWith orGate
    xor = zipWith xorGate
    complement = map notGate
    shiftL xs num =  drop num xs ++
      map ( const $ wrapConstant False) [0 .. num  -1]
    shiftR xs num =  map ( const $ wrapConstant False)
      [0 .. num  -1]  ++  take num xs
    rotate =  undefined
    -- rotate x st =  take ( length st)  $  drop ( negate x ` mod`  length st)  $  cycle st
    isSigned _ =  False
    testBit =  undefined
    bit _ =  undefined
    popCount =  undefined
    bitSize =  length
    bitSizeMaybe a =  Just $  length a

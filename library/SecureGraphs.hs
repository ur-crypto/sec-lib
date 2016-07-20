{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module SecureGraphs where

import           Data.List
import           Data.Bits

import qualified Data.Graph.Inductive.Graph        as G
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree

import           Control.Monad.State.Strict

data GateType = AND | OR | XOR | NOT deriving (Show, Eq)
data SecureNode = Gate GateType | Input | Const Bool deriving (Show, Eq)
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

wrapNode :: SecureNode -> SecureGraphBuilder
wrapNode n = Builder $ do
  enterGraph <- get
  let avail = head $ newNodes 1 enterGraph
  put $ insNode (avail, n) enterGraph
  return avail

wrapConstant :: Bool -> SecureGraphBuilder
wrapConstant = Constant

generateInputs :: Int -> (SecureNum, SecureGraph)
generateInputs numberInputs = (inputs, inputsGraph)
  where
    inputs = map (Builder . return) [1 .. numberInputs]
    inputsGraph = foldr (\n g -> G.insNode (n, Input) g) G.empty [1 .. numberInputs]

secure32 :: (SecureNum, SecureGraph)
secure32 = generateInputs 32

createProgramCircuit :: SecureFunction -> Int -> Int -> SecureGraph
createProgramCircuit func numberInputsA numberInputsB =
    let inputSize = numberInputsA + numberInputsB
        (startBuilders, startGraph) = generateInputs inputSize
        (aBuilders, bBuilders) = splitAt (numberInputsA) startBuilders
        builderResults = func aBuilders bBuilders in
      mergeResults startGraph builderResults

mergeResults :: SecureGraph -> SecureNum -> SecureGraph
mergeResults graph [] = graph
mergeResults graph (Builder result:results) = mergeResults (execState result graph) results
mergeResults graph (Constant result:results) =
  mergeResults (G.insNode (head $ newNodes 1 graph, Const result) graph) results

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

printGraph :: SecureGraph -> IO()
printGraph = prettyPrint

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module SecureGraphs where

import           Prelude                           hiding (not, (&&), (||))

import Data.List

import qualified Data.Graph.Inductive.Graph        as G

import           Control.Monad.State.Strict
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree

import           Data.Bits

data GateType = AND | OR | XOR | NOT deriving (Show, Eq)
data SecureNode = Gate GateType | Input | Constant Bool deriving (Show, Eq)
type SecureGraph = Gr SecureNode ()
type SecureGraphBuilder = (State SecureGraph Int)
type SecureGate = SecureGraphBuilder -> SecureGraphBuilder -> SecureGraphBuilder
type SecureNum = [SecureGraphBuilder]
type SecureFunction = SecureNum -> SecureNum -> SecureNum

isGate :: GateType -> SecureGraph -> Int -> Bool
isGate ty graph num =
  let mContext = fst $ match num graph in
    maybe False (\x -> Gate ty == lab' x) mContext

bigGate :: GateType -> SecureGate
bigGate ty leftM rightM = do
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
notGate m = do
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

buildGraph :: SecureGraphBuilder -> SecureGraph -> SecureGraph
buildGraph = execState

wrapNode :: SecureNode -> SecureGraphBuilder
wrapNode n = do
  enterGraph <- get
  let avail = head $ newNodes 1 enterGraph
  put $ insNode (avail, n) enterGraph
  return avail


wrapConstant :: Bool -> SecureGraphBuilder
wrapConstant = wrapNode . Constant

gInput :: SecureGraphBuilder
gInput = wrapNode Input

inputsGraph :: Int -> SecureGraph
inputsGraph num = foldr (\n g -> G.insNode (n, Input) g) G.empty [1 .. num]

inputs :: Int -> SecureNum
inputs num = map return [1 .. num]

secure32 :: ((SecureNum, SecureNum), SecureGraph)
secure32 = ((splitAt 32 $ inputs 64), inputsGraph 64)

-- numberInputs :: SecureNum -> Int
-- numberInputs = foldr (\graph n -> (+) n $ G.noNodes $ G.labnfilter (\x -> Input == snd x) (buildGraph graph)) 0

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
    -- shiftL xs num =  drop num xs ++
    --   map ( const $ wrapNode $ Constant False) [0 .. num  -1]
    -- shiftR xs num =  map ( const $ wrapNode $ Constant  False)
    --   [0 .. num  -1]  ++  take num xs
    rotate =  undefined
    -- rotate x st =  take ( length st)  $  drop ( negate x ` mod`  length st)  $  cycle st
    isSigned _ =  False
    testBit =  undefined
    bit _ =  undefined
    popCount =  undefined
    bitSize =  length
    bitSizeMaybe a =  Just $  length a

-- instance Show SecureGraphBuilder where
--   show = show . buildGraph

-- printFullGraph :: [SecureGraphBuilder] -> IO ()
-- printFullGraph = mapM_ $ prettyPrint . buildGraph

printGraph :: SecureGraph -> IO()
printGraph = prettyPrint

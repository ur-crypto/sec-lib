{-# LANGUAGE ScopedTypeVariables #-}
module SecureComputation where
import Debug.Trace

import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree

import           Control.Concurrent.Async

import           Control.Monad.Trans.Reader

import           Pipes
import           Pipes.Concurrent
import           Pipes.Lift

import           Types

data NodeConnectors = NodeConnectors{networkPair :: (Output Key, Input Key), nodePair :: (Output KeyType, Input KeyType)}
type ComputationGraph = Gr (SecureNode, NodeConnectors) (Output KeyType)

computeGraph ::
  SecureGraph ->
  ((Output Key, Input Key) -> GateType -> SecurePipe) ->
  ([(Output Key, Input Key)] -> Effect IO ()) ->
  SecurePipe
computeGraph graph processingFunction networkHandler = do
  let unlabNodes = nodes graph
  let origNodes = labNodes graph
  let origEdges = edges graph
  listHandles <- liftIO $ mapM (const $ spawn unbounded) unlabNodes
  networkHandles <- liftIO $ mapM (const $ spawn unbounded) unlabNodes
  let handles = zipWith NodeConnectors networkHandles listHandles
  let newLabs = zip (fst $ unzip origNodes) $ zip (snd $ unzip origNodes) handles
  let edgelessGraph :: ComputationGraph = insNodes newLabs empty
  let edgyGraph = foldl makeEdgyGraph edgelessGraph origEdges
  liftIO $ runEffect $ networkHandler networkHandles
  trace (prettify graph) $ ufold processGraph (return ()) edgyGraph
  where
    makeEdgyGraph grph (edger, edgee) =
      insEdge (edger, edgee, fst $ nodePair $ snd $ lab' $ context grph edger) grph
    processGraph ctext =
     trace "Processing Graph" $
     case (snd $ unzip $ lsuc' ctext, snd $ unzip $ lpre' ctext) of
        ([], []) -> mappend single
        (_, []) -> mappend output
        ([], _) -> mappend $ input >-> process
        (_, _) -> flip mappend $ do
          i <- lift ask
          liftIO $
            flip withAsync (const $ return ()) $
            runEffect $
            runReaderP i $
            input >-> process >-> output
        where
          input = fromInput $ snd $ nodePair $ snd $ lab' ctext
          output = toOutput $ mconcat $ snd $ unzip $ lsuc' ctext
          single = do
            let (Const b, _) = trace (show $ fst $ lab' ctext) $ lab' ctext
            _ <- await
            yield $ Constant b
          process =
            let (Gate ty, NodeConnectors{networkPair = n}) = lab' ctext in
              processingFunction n ty

{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SecureComputation where
-- import           Debug.Trace

-- import           Data.Graph.Inductive.Basic
-- import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
-- import           Data.Graph.Inductive.Query.DFS

-- import           Control.Concurrent.Async

-- import           Control.Monad.Trans.Reader

-- import           Pipes
import           Pipes.Concurrent
-- import           Pipes.Lift

import           Types                             hiding (Input)
-- import qualified Types                             as T

data NodeConnectors = NodeConnectors{networkPair :: (Output Key, Input Key), nodePair :: (Output KeyType, Input KeyType)}
type ComputationGraph = Gr (SecureNode, NodeConnectors) (Output KeyType)

-- makeComputationGraph :: SecureGraph -> [NodeConnectors] -> ComputationGraph
-- makeComputationGraph g handles =
--   let origNodes = labNodes g
--       origEdges = edges g
--       newLabs = zip (fst $ unzip origNodes) $ zip (snd $ unzip origNodes) handles
--       edgelessGraph :: ComputationGraph = insNodes newLabs empty in
--     foldl makeEdgyGraph edgelessGraph origEdges
--   where
--     makeEdgyGraph grph (edger, edgee) =
--       insEdge (edger, edgee, fst $ nodePair $ snd $ lab' $ context grph edger) grph
-- computeGraph ::
--   ComputableGraph ->
--   ((Output Key, Input Key) -> GateType -> SecurePipe) ->
--   SecurePipe
-- computeGraph ComputableGraph{graphNodes, graph} processingFunction = do
--   let unlabNodes = nodes graph
--   listHandles <- liftIO $ mapM (const $ spawn unbounded) unlabNodes
--   networkHandles <- liftIO $ mapM (const $ spawn unbounded) unlabNodes
--   let handles = zipWith NodeConnectors networkHandles listHandles
--   let compGraph = grev $ makeComputationGraph graph handles
--   trace (show $ dfs graphNodes compGraph) $
--     trace (prettify graph) $
--     dfsWith processGraph graphNodes compGraph
--   where
--     processGraph ctext =
--      trace "Processing Graph" $
--      case fst $ lab' ctext of
--        T.Input -> _
--        Gate ty -> _
--        Const b -> _
--      -- case (snd $ unzip $ lsuc' ctext, snd $ unzip $ lpre' ctext) of
--      --    ([], []) -> mappend single
--      --    (_, []) -> mappend output
--      --    ([], _) -> mappend $ input >-> process
--      --    (_, _) -> flip mappend $ do
--      --      i <- lift ask
--      --      liftIO $
--      --        flip withAsync (const $ return ()) $
--      --        runEffect $
--      --        runReaderP i $
--      --        input >-> process >-> output
--      --    where
--      --      input = fromInput $ snd $ nodePair $ snd $ lab' ctext
--      --      output = toOutput $ mconcat $ snd $ unzip $ lsuc' ctext
--      --      single = do
--      --        let (Const b, _) = trace (show $ fst $ lab' ctext) $ lab' ctext
--      --        _ <- await
--      --        yield $ Constant b
--      --      process =
--      --        let (Gate ty, NodeConnectors{networkPair = n}) = lab' ctext in
--      --          processingFunction n ty

module Regex.Util (relabelAndTranslate, addGraphsAndTranslate) where
-- relabelAndTranslate is only exported for testing purposes.
-- TODO: put it into an Internal module for testing.

import Data.Graph.Inductive.Graph (newNodes,buildGr,ufold,insEdges,insNode,delNode,Context(..),Node,labNodes)
import Data.Graph.Inductive.PatriciaTree (Gr(..))
import Data.List (elem,delete)
import qualified Data.Map.Strict as M
import Debug.Trace (trace) -- TODO remove

addGraphs :: Gr a b -> Gr a b -> Gr a b
addGraphs one two = smoosh one (relabel one two)

-- Takes nodes of interest from the 'extra' graph and translates their new labels.
addGraphsAndTranslate :: (Show a, Show b) => Gr a b -> (Gr a b,[Node]) -> (Gr a b, M.Map Node Node)
addGraphsAndTranslate base_graph (extra_graph, nodes_of_interest) =
    (smoosh base_graph new_graph, new_node_map)
  where (new_graph, new_node_map) = relabelAndTranslate base_graph (extra_graph, nodes_of_interest)

-- Puts two graphs together and doesn't <s>afraid of anything</s>
-- care whether they have overlapping labels
smoosh :: Gr a b -> Gr a b -> Gr a b
smoosh one two = buildGr (getContexts one ++ getContexts two)
  where getContexts = ufold (:) []

-- Relabels the second graph so it's compatible with the first
-- (has no overlapping labels)
relabel :: Gr a b -> Gr a b -> Gr a b
relabel one two = fst $ ufold relabelNode (two,next_nodes) two
  where relabelNode :: Context a b -> (Gr a b,[Node]) -> (Gr a b,[Node])
        relabelNode (in_edges, node, thing, out_edges) (graph,(next_node:rest_nodes)) =
            (insEdges (reformat_edges next_node in_edges) $
             insEdges (reformat_edges next_node out_edges) $
             delNode node $
             insNode (next_node, thing) graph,
             rest_nodes)
        reformat_edges next_node = map (\(edge_thing, other_node) -> (other_node, next_node, edge_thing))
        next_nodes = newNodes (length $ labNodes two) one

-- Relabels the second graph so it's compatible with the first.
-- Also:
-- Takes a list of nodes-of-interest from the second graph,
-- and returns their new indices in the resulting graph.
relabelAndTranslate :: (Show a, Show b) => Gr a b -> (Gr a b,[Node]) -> (Gr a b,M.Map Node Node)
relabelAndTranslate base_graph (graph_to_relabel, interesting_nodes) =
  trace "********* running relabelAndTranslate" $
  (relabeled_graph, relabeled_nodes)
  where RelabelContext { partlyRelabeledGraph = relabeled_graph, translatedNodes = relabeled_nodes } =
            ufold relabelNode initial_context graph_to_relabel
        initial_context = RelabelContext {
                               partlyRelabeledGraph = graph_to_relabel,
                               newNodeLabels = next_nodes_for_base_graph,
                               remainingNodesToTranslate = interesting_nodes,
                               translatedNodes = M.empty }
        next_nodes_for_base_graph = newNodes (length $ labNodes graph_to_relabel) base_graph

data RelabelContext a b = RelabelContext {
  partlyRelabeledGraph :: Gr a b,
  newNodeLabels :: [Node],
  remainingNodesToTranslate :: [Node],
  translatedNodes :: M.Map Node Node
}

relabelNode :: (Show a, Show b) => Context a b -> RelabelContext a b -> RelabelContext a b
relabelNode (in_edges_to_relabel, node_to_relabel, node_payload, out_edges_to_relabel)
  old_relabel_context = -- (graph_to_relabel, (next_node:rest_nodes), (old_nodes,new_nodes)) =
    trace ("$$ relabeling node " ++ show node_to_relabel ++
           " in graph " ++ show (partlyRelabeledGraph old_relabel_context)) $
    RelabelContext {
      partlyRelabeledGraph =
        if this_node_is_interesting
        then fix_this_node_edges $ partlyRelabeledGraph old_relabel_context
        else partlyRelabeledGraph old_relabel_context,

      -- By construction of the fold, this should never crash.
      newNodeLabels = new_available_nodes,
      -- By construction of the fold, this should never crash.
      remainingNodesToTranslate = tail $ remainingNodesToTranslate old_relabel_context,

      translatedNodes = translated_nodes
    }
  where (new_available_nodes, translated_nodes) =
          try_relabel_nodes node_to_relabel
                            next_available_node
                            (remainingNodesToTranslate old_relabel_context,
                            translatedNodes old_relabel_context)

        fix_this_node_edges old_graph =
          delNode node_to_relabel $ add_edges $ insNode (next_available_node, node_payload) $ old_graph

        add_edges = insEdges (reformat_edges next_available_node out_edges_to_relabel) .
                    insEdges (reformat_edges next_available_node in_edges_to_relabel)

        try_relabel_nodes :: Node -> Node -> ([Node],M.Map Node Node) -> ([Node],M.Map Node Node)
        try_relabel_nodes node next_node (old_nodes,new_node_map) = 
            if this_node_is_interesting
            then (delete node old_nodes, M.insert node next_node new_node_map)
            else (old_nodes,new_node_map)

        this_node_is_interesting = node_to_relabel `elem` (remainingNodesToTranslate old_relabel_context)

        -- By construction of the fold, this should never crash.
        next_available_node = head $ newNodeLabels old_relabel_context

        reformat_edges next_available_node =
            map (\(edge_payload, other_node_in_edge) -> (other_node_in_edge, next_available_node, edge_payload))

module Regex.Util (relabelAndTranslate, addGraphsAndTranslate) where
-- relabelAndTranslate is only exported for testing purposes.
-- TODO: put it into an Internal module for testing.

import Data.Graph.Inductive.Graph (newNodes,buildGr,ufold,insEdge,insNode,delNode,Context(..),Node,labNodes,nodes)
import Data.Graph.Inductive.PatriciaTree (Gr(..))
import Data.List (elem,delete)
import qualified Data.Map.Strict as M
import Debug.Trace (trace) -- TODO remove

-- Takes nodes of interest from the 'extra' graph and translates their new labels.
addGraphsAndTranslate :: (Show a, Show b) => Gr a b -> Gr a b -> (Gr a b, M.Map Node Node)
addGraphsAndTranslate base_graph extra_graph =
    (smoosh base_graph new_graph, new_node_map)
  where (new_graph, new_node_map) = relabelAndTranslate base_graph extra_graph

-- Puts two graphs together and doesn't <s>afraid of anything</s>
-- care whether they have overlapping labels
smoosh :: Gr a b -> Gr a b -> Gr a b
smoosh one two = buildGr (getContexts one ++ getContexts two)
  where getContexts = ufold (:) []

-- Relabels the second graph so it's compatible with the first.
-- Also: Returns a map from old node labels in the second graph, to their new labels in the combined graph.
-- TODO: Make the above line of documentation true (so far it still just takes a list of nodes of interest).
-- Labels for the first graph stay the same in the combined graph.
relabelAndTranslate :: (Show a, Show b) => Gr a b -> Gr a b -> (Gr a b,M.Map Node Node)
relabelAndTranslate base_graph graph_to_relabel =
  trace "********* running relabelAndTranslate" $
  (relabeled_graph, relabeled_nodes)
  where RelabelContext { partlyRelabeledGraph = relabeled_graph, translatedNodes = relabeled_nodes } =
            ufold relabelNode initial_context graph_to_relabel
        initial_context = RelabelContext {
                               partlyRelabeledGraph = graph_to_relabel,
                               translatedNodes = constructNewNodeLabels base_graph graph_to_relabel }

data RelabelContext a b = RelabelContext {
  partlyRelabeledGraph :: Gr a b,
  translatedNodes :: M.Map Node Node
}

-- Takes a base graph and a graph to extend it with, and returns a map from:
-- extra_graph nodes -> new labels for those nodes in the combined graph.
constructNewNodeLabels :: Gr a b -> Gr a b -> M.Map Node Node
constructNewNodeLabels base_graph extra_graph =
  foldl add_tuple_to_map M.empty zipped_translation_tuples
  where add_tuple_to_map :: M.Map Node Node -> (Node, Node) -> M.Map Node Node
        add_tuple_to_map map_ (old_label, new_label) = M.insert old_label new_label map_
        zipped_translation_tuples = zip old_node_labels (newNodes (length $ old_node_labels) base_graph)
        old_node_labels = nodes extra_graph

relabelNode :: (Show a, Show b) => Context a b -> RelabelContext a b -> RelabelContext a b
relabelNode (in_edges_to_relabel, node_to_relabel, node_payload, out_edges_to_relabel)
  old_relabel_context = -- (graph_to_relabel, (next_node:rest_nodes), (old_nodes,new_nodes)) =
    trace ("$$ relabeling node " ++ show node_to_relabel ++
           " in graph " ++ show (partlyRelabeledGraph old_relabel_context) ++
           " with in edges " ++ show in_edges_to_relabel ++
           " and out edges " ++ show out_edges_to_relabel) $
    old_relabel_context {
      -- Does some extra work if we don't need to relabel the node
      -- (deletes and reinserts it)
      partlyRelabeledGraph = 
        delNode node_to_relabel $
        fix_all_edges $
        insNode (translate_node node_to_relabel, node_payload) $
        partlyRelabeledGraph old_relabel_context
    }
  where fix_all_edges old_graph =
          foldl fix_out_edge (foldl fix_in_edge old_graph in_edges_to_relabel) out_edges_to_relabel
          
        -- Edges that are into 'node_to_relabel'
        fix_in_edge :: Gr a b -> (b, Node) -> Gr a b
        fix_in_edge graph (edge_payload, other_node_in_edge) =
          insEdge (translate_node other_node_in_edge, translate_node node_to_relabel, edge_payload) graph

        -- Edges that are outward from 'node_to_relabel'
        fix_out_edge :: Gr a b -> (b, Node) -> Gr a b
        fix_out_edge graph (edge_payload, other_node_in_edge) =
          insEdge (translate_node node_to_relabel, translate_node other_node_in_edge, edge_payload) graph
        
        translate_node node = M.findWithDefault node node (translatedNodes old_relabel_context)

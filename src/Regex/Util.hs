module Regex.Util (relabelAndTranslate) where

import Data.Graph.Inductive.Graph (newNodes,buildGr,ufold,insEdges,insNode,delNode,Context(..),Node,labNodes)
import Data.Graph.Inductive.PatriciaTree (Gr(..))
import Data.List (elem,delete)
import qualified Data.Map.Strict as M
import Debug.Trace (trace) -- TODO remove

addGraphs :: Gr a b -> Gr a b -> Gr a b
addGraphs one two = smoosh one (relabel one two)

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

-- Takes a list of nodes-of-interest from the second graph,
-- and returns their new indices in the resulting graph.
relabelAndTranslate :: (Show a, Show b) => Gr a b -> (Gr a b,[Node]) -> (Gr a b,M.Map Node Node)
relabelAndTranslate one (two,interesting_nodes) =
  (relabeled_graph, relabeled_nodes)
  where (relabeled_graph, _, (_,relabeled_nodes)) =
            ufold relabelNode (two,next_nodes,(interesting_nodes,M.empty)) two 
        next_nodes = newNodes (length $ labNodes two) one

-- Factored out for... reasons
relabelNode :: (Show a, Show b) => Context a b -> (Gr a b, [Node], ([Node],M.Map Node Node))
    -> (Gr a b, [Node], ([Node],M.Map Node Node))
relabelNode (in_edges, node, thing, out_edges)
  (graph,(next_node:rest_nodes), (old_nodes,new_nodes)) =
    trace ("relabeling node " ++ show node ++ " in graph " ++ show graph) $
    (insEdges (reformat_edges next_node in_edges) $
     insEdges (reformat_edges next_node out_edges) $
     delNode node $
     insNode (next_node, thing) graph,
     rest_nodes,
     try_relabel_nodes node next_node (old_nodes,new_nodes))
  where try_relabel_nodes :: Node -> Node -> ([Node],M.Map Node Node) -> ([Node],M.Map Node Node)
        try_relabel_nodes node next_node (old_nodes,new_node_map) = 
            if node `elem` old_nodes
            then (delete node old_nodes, M.insert node next_node new_node_map)
            else (old_nodes,new_node_map)

        reformat_edges next_node = map (\(edge_thing, other_node) -> (other_node, next_node, edge_thing))

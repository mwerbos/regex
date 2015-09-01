module Regex.Util (addGraphs) where

import Data.Graph.Inductive.Graph (newNodes,buildGr,ufold,insEdges,insNode,delNode,Context(..),Node,labNodes)
import Data.Graph.Inductive.PatriciaTree (Gr(..))

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

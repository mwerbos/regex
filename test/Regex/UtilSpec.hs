module Regex.UtilSpec where

import SpecHelper
import Regex.Util
import Control.Exception (evaluate)
import Data.Graph.Inductive.Graph (mkGraph)
import qualified Data.Map as M

spec :: Spec
spec = do
  describe "relabelAndTranslate" $ do
    it "relabels two small graphs" $ do
      let base_graph = mkGraph [(0,()), (1,())] [(0,1,Single 'h')]
          graph_to_relabel = mkGraph [(0,()), (1,())] [(0,1,Single 'i')]
      relabelAndTranslate base_graph graph_to_relabel `shouldBe`
          (mkGraph [(2,()), (3,())]
                   [(2,3,Single 'i')],
           M.fromList [(0,2),(1,3)])
  describe "addGraphsAndTranslate" $ do
    it "adds two small graphs" $ do
      let base_graph = mkGraph [(0,()), (1,())] [(0,1,Single 'h')]
          graph_to_relabel = mkGraph [(0,()), (1,())] [(0,1,Single 'i')]
      addGraphsAndTranslate base_graph graph_to_relabel `shouldBe`
          (mkGraph [(0,()), (1,()), (2,()), (3,())]
                   [(0,1,Single 'h'), (2,3,Single 'i')],
           M.fromList [(0,2), (1,3)])

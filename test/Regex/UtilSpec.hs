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
      relabelAndTranslate base_graph (graph_to_relabel, [0]) `shouldBe`
          (mkGraph [(2,()), (3,())]
                   [(2,3,Single 'i')],
           M.insert 0 2 $ M.empty)

main :: IO ()
main = hspec spec

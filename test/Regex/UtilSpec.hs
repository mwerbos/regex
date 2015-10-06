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
      let graph1 = mkGraph [(0,()), (1,())] [(0,1,Single 'h')]
          graph2 = mkGraph [(0,()), (1,())] [(0,1,Single 'i')]
      relabelAndTranslate graph1 (graph2, [0]) `shouldBe`
          (mkGraph [(0,()), (1,()), (2,()), (3,())]
                   [(0,1,Single 'h'), (2,3,Single 'i')],
           M.insert 0 2 $ M.empty)

main :: IO ()
main = hspec spec

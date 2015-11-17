module Regex.RunAutomatonSpec where

import SpecHelper
import Regex.RunAutomaton
import Control.Exception (evaluate)
import Data.Graph.Inductive.Graph (mkGraph)

spec :: Spec
spec = do
  describe "runAutomaton" $ do
    it "runs a small automaton on a short string" $ do
      let simple_automaton = Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,())]
                               [(0,1,T $ Single 'f'), (1,2,Epsilon), (2,3,T $ Single 'o'),
                                (3,4,Epsilon), (4,5,T $ Single 'x')],
            finalState = 5
          }
      runAutomaton simple_automaton "the fox was " `shouldBe` [Interval (4, 7)]

main :: IO ()
main = hspec spec

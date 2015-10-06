module Regex.CompileSpec where

import SpecHelper
import Regex.Compile
import Control.Exception (evaluate)
import Data.Graph.Inductive.Graph (mkGraph)

spec :: Spec
spec = do
  describe "tokenize" $ do
    it "tokenizes a simple expression" $ do
      let regex = Regex "hi"
      tokenize regex `shouldBe` [('h', OtherChar), ('i', OtherChar)]
  describe "parse" $ do
    it "parses a simple expression" $ do
      let tokenized = [('f', OtherChar), ('o', OtherChar), ('x', OtherChar)]
      parse tokenized `shouldBe` [Single 'f', Single 'o', Single 'x']
  describe "build automaton" $ do
    it "builds a simple automaton" $ do
      let parsed = [Single 'h', Single 'i']
          simple_automaton = Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,())]
                               [(0,1,T $ Single 'h'), (1,2,Epsilon), (2,3,T $ Single 'i')],
            finalState = 3
          }
      makeAutomaton parsed `shouldBe` simple_automaton

main :: IO ()
main = hspec spec

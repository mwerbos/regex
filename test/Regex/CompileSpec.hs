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
  describe "makeMiniAutomaton" $ do
    it "makes a mini automaton with a character class" $ do
      let token = Group [Single 'y', Single 'd']
          simple_automaton = Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,())]
                               [(0,2,Epsilon), (0,4,Epsilon),
                                (2,3,T $ Single 'y'), (4,5,T $ Single 'd'),
                                (3,1,Epsilon), (5,1,Epsilon)],
            finalState = 1
          }
      makeMiniAutomaton token `shouldBe` simple_automaton
  describe "build automaton" $ do
    it "builds a simple automaton" $ do
      let parsed = [Single 'h', Single 'i']
          simple_automaton = Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,())]
                               [(0,1,T $ Single 'h'), (1,2,Epsilon), (2,3,T $ Single 'i')],
            finalState = 3
          }
      makeAutomaton parsed `shouldBe` simple_automaton
    it "builds an automaton with a character class" $ do
      let parsed = [Group [Single 'y', Single 'd'], Single 'o']
          simple_automaton = Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,()), (6,()), (7,())]
                               [(0,2,Epsilon), (0,4,Epsilon),
                                (2,3,T $ Single 'y'), (4,5,T $ Single 'd'),
                                (3,1,Epsilon), (5,1,Epsilon),
                                (1,6,Epsilon), (6,7,T $ Single 'o')],
            finalState = 7
          }
      makeAutomaton parsed `shouldBe` simple_automaton
    it "builds a longer automaton" $ do
      let parsed = [Single 'f', Single 'o', Single 'x']
          simple_automaton = Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,())]
                               [(0,1,T $ Single 'f'), (1,2,Epsilon), (2,3,T $ Single 'o'),
                                (3,4,Epsilon), (4,5,T $ Single 'x')],
            finalState = 5
          }
      makeAutomaton parsed `shouldBe` simple_automaton
  describe "orAutomaton" $ do
    it "combines two simple automatons" $ do
      let first = Automaton {
            stateMap = mkGraph [(0,()), (1,())] [(0,1,T $ Single 'h')],
            finalState = 1
          }
          second = Automaton {
            stateMap = mkGraph [(0,()), (1,())] [(0,1,T $ Single 'i')],
            finalState = 1
          }
      orAutomatons first second `shouldBe` Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,())]
                               [(0,2,Epsilon), (0,4,Epsilon),
                                (2,3,T $ Single 'h'), (4,5,T $ Single 'i'),
                                (3,1,Epsilon), (5,1,Epsilon)],
            finalState = 1
          }
    it "has an identity of 'emptyAutomaton'" $ do
      let aut = Automaton {
            stateMap = mkGraph [(0,()), (1,())] [(0,1,T $ Single 'h')],
            finalState = 1
          }
      orAutomatons aut emptyAutomaton `shouldBe` aut
      orAutomatons emptyAutomaton aut `shouldBe` aut
  describe "combineThreeGraphs" $ do
    it "combines three graphs correctly" $ do
      let first = mkGraph [(0,()), (1,())] [(0,1,T $ Single 'h')]
          second = mkGraph [(0,()), (1,())] [(0,1,T $ Single 'i')]
          third = mkGraph [(0,()), (1,())] [(0,1,T $ Single 'd')]
          (combined, (fn1, fn2, fn3)) = combineThreeGraphs (first, second, third)
      combined `shouldBe` mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,())]
                                  [(0,1,T $ Single 'h'), (2,3,T $ Single 'i'), (4,5,T $ Single 'd')]
      fn1 0 `shouldBe` 0
      fn1 1 `shouldBe` 1
      fn2 0 `shouldBe` 2
      fn2 1 `shouldBe` 3
      fn3 0 `shouldBe` 4
      fn3 1 `shouldBe` 5

main :: IO ()
main = hspec spec

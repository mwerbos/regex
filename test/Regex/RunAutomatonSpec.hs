module Regex.RunAutomatonSpec where

import SpecHelper
import Regex.RunAutomaton
import Control.Exception (evaluate)
import Data.Graph.Inductive.Graph (mkGraph)

simple_automaton :: Automaton
simple_automaton = Automaton {
  stateMap = mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,())]
                     [(0,1,T $ Single 'f'), (1,2,Epsilon), (2,3,T $ Single 'o'),
                      (3,4,Epsilon), (4,5,T $ Single 'x')],
  finalState = 5
}

spec :: Spec
spec = do
  describe "runAutomaton" $ do
    it "runs a small automaton on a short string" $ do
      runAutomaton simple_automaton "the fox was " `shouldBe` [Interval (4, 7)]

  describe "runAutomatonOnce" $ do
    it "moves to the next state on seeing the right character" $ do
      runAutomatonOnce simple_automaton initialState 'f' `shouldBe`
          ProcessingState {
            possibleMatches = [P {matchState = 1, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = []
          }
    it "does not move on after seeing a wrong character" $ do
      runAutomatonOnce simple_automaton initialState 'r' `shouldBe` initialState

  describe "runStatesOnce" $ do
    it "runs a small automaton on a single character" $ do
      runStatesOnce simple_automaton initialState 'f' `shouldBe`
          ProcessingState {
            possibleMatches = [P {matchState = 1, startIndex = 0 }],
            currentIndex = 0,
            currentMatches = []
          }
    it "does not move on after seeing a wrong character" $ do
      runStatesOnce simple_automaton initialState 'r' `shouldBe` initialState
  describe "runNonEpsilonMoves" $ do
    it "runs a small automaton on a single character" $ do
      runNonEpsilonMoves simple_automaton initialState 'f' `shouldBe`
          ProcessingState {
            possibleMatches = [P {matchState = 1, startIndex = 0 }],
            currentIndex = 0,
            currentMatches = []
          }
    it "does not move on after seeing a wrong character" $ do
      runNonEpsilonMoves simple_automaton initialState 'r' `shouldBe` initialState

main :: IO ()
main = hspec spec

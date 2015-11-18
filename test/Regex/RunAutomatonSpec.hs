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
  describe "runAutomatonOnce" $ do
    it "runs a small automaton on a single character" $ do
      let simple_automaton = Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,())]
                               [(0,1,T $ Single 'f'), (1,2,Epsilon), (2,3,T $ Single 'o'),
                                (3,4,Epsilon), (4,5,T $ Single 'x')],
            finalState = 5
          }
          processing_state = initialState
      runAutomatonOnce simple_automaton processing_state 'f' `shouldBe`
          ProcessingState {
            possibleMatches = [P {matchState = 1, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = []
          }
      -- No change if we hit a state that is not useful.
      runAutomatonOnce simple_automaton processing_state 'r' `shouldBe` processing_state
  describe "runStatesOnce" $ do
    it "runs a small automaton on a single character" $ do
      let simple_automaton = Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,())]
                               [(0,1,T $ Single 'f'), (1,2,Epsilon), (2,3,T $ Single 'o'),
                                (3,4,Epsilon), (4,5,T $ Single 'x')],
            finalState = 5
          }
          processing_state = initialState
      runStatesOnce simple_automaton processing_state 'f' `shouldBe`
          ProcessingState {
            possibleMatches = [P {matchState = 1, startIndex = 0 }],
            currentIndex = 0,
            currentMatches = []
          }
      -- No change if we hit a state that is not useful.
      runStatesOnce simple_automaton processing_state 'r' `shouldBe` processing_state
  describe "runNonEpsilonMoves" $ do
    it "runs a small automaton on a single character" $ do
      let simple_automaton = Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,())]
                               [(0,1,T $ Single 'f'), (1,2,Epsilon), (2,3,T $ Single 'o'),
                                (3,4,Epsilon), (4,5,T $ Single 'x')],
            finalState = 5
          }
          processing_state = initialState
      runNonEpsilonMoves simple_automaton processing_state 'f' `shouldBe`
          ProcessingState {
            possibleMatches = [P {matchState = 1, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = []
          }
      -- No change if we hit a state that is not useful.
      runNonEpsilonMoves simple_automaton processing_state 'r' `shouldBe` processing_state

main :: IO ()
main = hspec spec

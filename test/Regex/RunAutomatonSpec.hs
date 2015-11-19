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

-- Automaton with a character class
or_automaton :: Automaton
or_automaton = Automaton {
  stateMap = mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,()), (6,()), (7,())]
                     [(0,2,Epsilon), (0,4,Epsilon),
                      (2,3,T $ Single 'y'), (4,5,T $ Single 'd'),
                      (3,1,Epsilon), (5,1,Epsilon),
                      (1,6,Epsilon), (6,7,T $ Single 'o')],
  finalState = 7
}

spec :: Spec
spec = do
  describe "runAutomaton" $ do
    it "runs a small automaton on a short string" $ do
      runAutomaton simple_automaton "the fox was " `shouldBe` [Interval (4, 7)]
    it "runs the automaton on the minimal string" $ do
      runAutomaton simple_automaton "fox" `shouldBe` [Interval (0, 3)]
    it "runs the automaton on the a slightly less minimal string" $ do
      runAutomaton simple_automaton " fox" `shouldBe` [Interval (1, 4)]

  describe "runAutomatonOnce" $ do
    it "moves to the next state on seeing the right character" $ do
      runAutomatonOnce simple_automaton initialState 'f' `shouldBe`
          ProcessingState {
            possibleMatches = [P {matchState = 0, startIndex = 1 },
                               P {matchState = 1, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = []
          }
    it "moves to the next state on seeing an or'ed character" $ do
      runAutomatonOnce or_automaton initialState 'y' `shouldBe`
          ProcessingState {
            possibleMatches = [P {matchState = 0, startIndex = 1 },
                               P {matchState = 3, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = []
          }
    it "moves to the next state after seeing an or'ed character" $ do
      let first_state = ProcessingState {
            possibleMatches = [P {matchState = 0, startIndex = 1 },
                               P {matchState = 3, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = []
          }
      runAutomatonOnce or_automaton first_state 'o' `shouldBe`
          ProcessingState {
            possibleMatches = [],
            currentIndex = 2,
            currentMatches = [Interval (0,2)]
          }
    it "moves to the end state on seeing the right character" $ do
      let almostEndState = ProcessingState {
            possibleMatches = [P {matchState = 4, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = []
          }
      runAutomatonOnce simple_automaton almostEndState 'x' `shouldBe`
          ProcessingState {
            possibleMatches = [P {matchState = 0, startIndex = 2 }],
            currentIndex = 2,
            currentMatches = [Interval (0, 2)]
          }
    it "does not move on after seeing a wrong character" $ do
      runAutomatonOnce simple_automaton initialState 'r' `shouldBe`
          initialState { possibleMatches = [P {matchState = 0, startIndex = 1 }],
                         currentIndex = 1 }

  describe "runStatesOnce" $ do
    it "runs a small automaton on a single character" $ do
      runStatesOnce simple_automaton initialState 'f' `shouldBe`
          ProcessingState {
            possibleMatches = [P {matchState = 1, startIndex = 0 }],
            currentIndex = 0,
            currentMatches = []
          }
    it "does not move on after seeing a wrong character" $ do
      runStatesOnce simple_automaton initialState 'r' `shouldBe`
          initialState { possibleMatches = [] }
    it "moves on after seeing an or'ed character" $ do
      let first_state = ProcessingState {
            possibleMatches = [P {matchState = 0, startIndex = 1 },
                               P {matchState = 3, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = []
          }
      runStatesOnce or_automaton first_state 'o' `shouldBe`
          ProcessingState {
            possibleMatches = [P {matchState = 7, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = []
          }

  describe "runEpsilonMoves" $ do
    it "correctly moves automaton after seeing an or'ed character" $ do
      let first_state = ProcessingState {
            possibleMatches = [P {matchState = 0, startIndex = 1 },
                               P {matchState = 3, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = []
          }
      runEpsilonMoves or_automaton first_state `shouldBe`
          ProcessingState {
            possibleMatches = [P {matchState = 0, startIndex = 1},
                               P {matchState = 3, startIndex = 0},
                               P {matchState = 2, startIndex = 1},
                               P {matchState = 4, startIndex = 1},
                               P {matchState = 1, startIndex = 0},
                               P {matchState = 6, startIndex = 0}],
            currentIndex = 1,
            currentMatches = []
          }

  describe "runNonEpsilonMoves" $ do
    it "runs a small automaton on a single character" $ do
      runNonEpsilonMoves simple_automaton initialState 'f' `shouldBe`
          ProcessingState {
            possibleMatches = [P {matchState = 1, startIndex = 0 }],
            currentIndex = 0,
            currentMatches = []
          }
    it "does not move on after seeing a wrong character" $ do
      runNonEpsilonMoves simple_automaton initialState 'r' `shouldBe`
          initialState { possibleMatches = [] }

  describe "popFinalStates" $ do
    it "grabs a single final state" $ do
      let state_with_unpopped_match = ProcessingState {
            possibleMatches = [P {matchState = 2, startIndex = 0}],
            currentIndex = 2,
            currentMatches = []
          }
          state_with_popped_match = ProcessingState {
            possibleMatches = [],
            currentIndex = 2,
            currentMatches = [Interval (0, 2)]
          } 
      popFinalStates 2 state_with_unpopped_match `shouldBe` state_with_popped_match

main :: IO ()
main = hspec spec

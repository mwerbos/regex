module Regex.RunAutomatonSpec where

import SpecHelper
import Regex.RunAutomaton
import Control.Exception (evaluate)
import Data.Graph.Inductive.Graph (mkGraph,Node)
import qualified Data.Set as S

nLengthNodes :: Int -> [(Node,())]
nLengthNodes n = take n $ zip [0..] (repeat ())

simple_automaton :: Automaton
simple_automaton = Automaton {
  stateMap = mkGraph (nLengthNodes 6)
                     [(0,1,T $ Single 'f'), (1,2,Epsilon), (2,3,T $ Single 'o'),
                      (3,4,Epsilon), (4,5,T $ Single 'x')],
  finalState = 5
}

-- Automaton with a character class
or_automaton :: Automaton
or_automaton = Automaton {
  stateMap = mkGraph (nLengthNodes 8)
                     [(0,2,Epsilon), (0,4,Epsilon),
                      (2,3,T $ Single 'y'), (4,5,T $ Single 'd'),
                      (3,1,Epsilon), (5,1,Epsilon),
                      (1,6,Epsilon), (6,7,T $ Single 'o')],
  finalState = 7
}

-- Automaton with a negated character class
neg_automaton :: Automaton
neg_automaton = Automaton {
  stateMap = mkGraph (nLengthNodes 8)
                     [(0,2,Epsilon), (0,4,Epsilon),
                      (2,3,T $ NegChar 'y'), (4,5,T $ NegChar 'd'),
                      (3,1,Epsilon), (5,1,Epsilon),
                      (1,6,Epsilon), (6,7,T $ Single 'o')],
  finalState = 7
}

-- Automaton with two character classes
yob_dog_automaton :: Automaton
yob_dog_automaton = Automaton {
  stateMap = mkGraph (nLengthNodes 14)
                     [(0,2,Epsilon), (0,4,Epsilon),
                      (2,3,T $ Single 'y'), (4,5,T $ Single 'd'),
                      (3,1,Epsilon), (5,1,Epsilon),
                      (1,6,Epsilon), (6,7,T $ Single 'o'),
                      (7,8,Epsilon),
                      (8,10,Epsilon), (8,12,Epsilon),
                      (10,11,T $ Single 'g'), (12,13,T $ Single 'b'),
                      (11,9,Epsilon), (13,9,Epsilon)],
  finalState = 9
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

    it "runs an automaton with multiple character classes" $ do
      runAutomaton yob_dog_automaton "dog" `shouldBe` [Interval (0, 3)]

  describe "runAutomatonOnce" $ do
    it "moves to the next state on seeing the right character" $ do
      runAutomatonOnce simple_automaton initialState 'f' `shouldBe`
          ProcessingState {
            possibleMatches = S.fromList [P {matchState = 0, startIndex = 1 },
                                          P {matchState = 2, startIndex = 0 },
                                          P {matchState = 1, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = S.empty
          }
    it "moves to the next state on seeing an or'ed character" $ do
      runAutomatonOnce or_automaton initialState 'y' `shouldBe`
          ProcessingState {
            possibleMatches = S.fromList [P {matchState = 0, startIndex = 1 },
                                          P {matchState = 3, startIndex = 0 },
                                          P {matchState = 1, startIndex = 0 },
                                          P {matchState = 6, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = S.empty
          }
    it "moves to the next state(s) on NOT seeing a negated character" $ do
      runAutomatonOnce neg_automaton initialState 'g' `shouldBe`
          ProcessingState {
            possibleMatches = S.fromList [P {matchState = 0, startIndex = 1 },
                                          P {matchState = 3, startIndex = 0 },
                                          P {matchState = 1, startIndex = 0 },
                                          P {matchState = 6, startIndex = 0 },
                                          P {matchState = 5, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = S.empty
          }
    it "moves to the next state after seeing an or'ed character" $ do
      let first_state = ProcessingState {
            possibleMatches = S.fromList [P {matchState = 0, startIndex = 1 },
                               P {matchState = 3, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = S.empty 
          }
      runAutomatonOnce or_automaton first_state 'o' `shouldBe`
          ProcessingState {
            possibleMatches = S.fromList [P {matchState = 0, startIndex = 2 }],
            currentIndex = 2,
            currentMatches = S.fromList [Interval (0,2)]
          }
    it "moves to the end state in the middle of a complex automaton" $ do
      let first_state = ProcessingState {
            possibleMatches = S.fromList [P {matchState = 7, startIndex = 0 },
                                          P {matchState = 8, startIndex = 0 }],
            currentIndex = 2,
            currentMatches = S.empty
          }
      runAutomatonOnce yob_dog_automaton first_state 'g' `shouldBe`
        ProcessingState {
          possibleMatches = S.fromList [P {matchState = 0, startIndex = 3 },
                                        P {matchState = 11, startIndex = 0 }],
          currentIndex = 3,
          currentMatches = S.fromList [Interval (0,3)]
        }

    it "moves to the end state on seeing the right character" $ do
      let almostEndState = ProcessingState {
            possibleMatches = S.fromList [P {matchState = 4, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = S.empty
          }
      runAutomatonOnce simple_automaton almostEndState 'x' `shouldBe`
          ProcessingState {
            possibleMatches = S.fromList [P {matchState = 0, startIndex = 2 }],
            currentIndex = 2,
            currentMatches = S.fromList [Interval (0, 2)]
          }
    it "does not move on after seeing a wrong character" $ do
      runAutomatonOnce simple_automaton initialState 'r' `shouldBe`
          initialState { possibleMatches = S.fromList [P {matchState = 0, startIndex = 1 }],
                         currentIndex = 1 }

  describe "runStatesOnce" $ do
    it "runs a small automaton on a single character" $ do
      runStatesOnce simple_automaton 'f' initialState `shouldBe`
          ProcessingState {
            possibleMatches = S.fromList [P {matchState = 1, startIndex = 0 },
                                          P {matchState = 2, startIndex = 0 }],
            currentIndex = 0,
            currentMatches = S.empty 
          }
    it "does not move on after seeing a wrong character" $ do
      runStatesOnce simple_automaton 'r' initialState `shouldBe`
          initialState { possibleMatches = S.empty }
    it "moves on after seeing an or'ed character" $ do
      let first_state = ProcessingState {
            possibleMatches = S.fromList [P {matchState = 0, startIndex = 1 },
                               P {matchState = 3, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = S.empty
          }
      runStatesOnce or_automaton 'o' first_state `shouldBe`
          ProcessingState {
            possibleMatches = S.fromList [P {matchState = 7, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = S.empty
          }

  describe "runEpsilonMoves" $ do
    it "correctly moves automaton after seeing an or'ed character" $ do
      let first_state = ProcessingState {
            possibleMatches = S.fromList [P {matchState = 0, startIndex = 1 },
                                          P {matchState = 3, startIndex = 0 }],
            currentIndex = 1,
            currentMatches = S.empty
          }
      runEpsilonMoves or_automaton first_state `shouldBe`
          ProcessingState {
            possibleMatches = S.fromList [P {matchState = 0, startIndex = 1},
                               P {matchState = 3, startIndex = 0},
                               P {matchState = 2, startIndex = 1},
                               P {matchState = 4, startIndex = 1},
                               P {matchState = 1, startIndex = 0},
                               P {matchState = 6, startIndex = 0}],
            currentIndex = 1,
            currentMatches = S.empty
          }

  describe "runNonEpsilonMoves" $ do
    it "runs a small automaton on a single character" $ do
      runNonEpsilonMoves simple_automaton 'f' initialState `shouldBe`
          ProcessingState {
            possibleMatches = S.fromList [P {matchState = 1, startIndex = 0 }],
            currentIndex = 0,
            currentMatches = S.empty
          }
    it "does not move on after seeing a wrong character" $ do
      runNonEpsilonMoves simple_automaton 'r' initialState `shouldBe`
          initialState { possibleMatches = S.empty }

  describe "popFinalStates" $ do
    it "grabs a single final state" $ do
      let state_with_unpopped_match = ProcessingState {
            possibleMatches = S.fromList [P {matchState = 2, startIndex = 0}],
            currentIndex = 2,
            currentMatches = S.empty
          }
          state_with_popped_match = ProcessingState {
            possibleMatches = S.empty,
            currentIndex = 2,
            currentMatches = S.fromList [Interval (0, 2)]
          } 
      popFinalStates 2 state_with_unpopped_match `shouldBe` state_with_popped_match

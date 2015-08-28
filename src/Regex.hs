module Regex (Regex(..), matchExpression, Interval(..)) where

import Regex.Data
import Regex.Preprocess

matchExpression :: Regex -> String -> [Interval]
matchExpression regex string = runAutomaton automaton string
  where automaton = processRegex regex

runAutomaton :: Automaton -> String -> [Interval]
runAutomaton automaton string = currentMatches end_state
  where end_state = foldl (runAutomatonOnce automaton) initialState string

data PossibleMatch = P { state :: State, startIndex :: Int }

data ProcessingState = ProcessingState {
    possibleMatches :: [PossibleMatch],
    currentIndex :: Int,
    currentMatches :: [Interval]
}

initialMatch :: PossibleMatch
initialMatch = P { state = S 0, startIndex = 0 }

initialState :: ProcessingState
initialState = ProcessingState {
    possibleMatches = [initialMatch],
    currentIndex = 0,
    currentMatches = []
}

runAutomatonOnce :: Automaton -> ProcessingState -> Char -> ProcessingState
runAutomatonOnce automaton state char = popFinalStates (runStatesOnce automaton state char)
        -- First get new states by running the automaton
        -- Then pop any final states onto the intervals list

-- Runs the automaton over every current possible match, with the given character
runStatesOnce :: Automaton -> ProcessingState -> Char -> ProcessingState
runStatesOnce = error "runStatesOnce undefined" 

-- Removes all states that are the final state and turns them into matches
popFinalStates :: ProcessingState -> ProcessingState
popFinalStates = error "popFinalStates undefined"

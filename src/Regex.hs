module Regex (Regex(..), matchExpression, Interval(..)) where

import Regex.Data
import Regex.Compile

matchExpression :: Regex -> String -> [Interval]
matchExpression regex string = runAutomaton automaton string
  where automaton = processRegex regex

runAutomaton :: Automaton -> String -> [Interval]
runAutomaton automaton string = currentMatches end_state
  where end_state = foldl (runAutomatonOnce automaton) initialState string

data PossibleMatch = P { matchState :: State, startIndex :: Int }

data ProcessingState = ProcessingState {
    possibleMatches :: [PossibleMatch],
    currentIndex :: Int,
    currentMatches :: [Interval],
    endState :: State -- End state of the automaton. Probably doesn't belong here.
}

initialMatch :: PossibleMatch
initialMatch = P { matchState = S 0, startIndex = 0 }

initialState :: ProcessingState
initialState = ProcessingState {
    possibleMatches = [initialMatch],
    currentIndex = 0,
    currentMatches = []
}

runAutomatonOnce :: Automaton -> ProcessingState -> Char -> ProcessingState
runAutomatonOnce automaton state char = popFinalStates $ incrementIndex (runStatesOnce automaton state char)
        -- First get new states by running the automaton
        -- Then pop any final states onto the intervals list

incrementIndex :: ProcessingState -> ProcessingState
incrementIndex state = state { currentIndex = (currentIndex state) + 1 }

-- Runs the automaton over every current possible match, with the given character
runStatesOnce :: Automaton -> ProcessingState -> Char -> ProcessingState
runStatesOnce = error "runStatesOnce undefined" 

-- Removes all states that are the final state and turns them into matches
popFinalStates :: ProcessingState -> ProcessingState
popFinalStates state = state { 
    possibleMatches = new_possibilities,
    currentMatches = (currentMatches state) ++ new_matches
} where new_possibilities = filter (not . isEnd) (possibleMatches state)
        new_matches = map toInterval $ filter isEnd (possibleMatches state)

        isEnd :: PossibleMatch -> Bool
        isEnd p = matchState p == endState state
        
        toInterval :: PossibleMatch -> Interval
        toInterval (P {startIndex = i}) = Interval (i, currentIndex state)

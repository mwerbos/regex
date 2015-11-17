module Regex.RunAutomaton (runAutomaton) where

import Regex.Data
import Regex.Match

import Data.Graph.Inductive.PatriciaTree(Gr(..))
import Data.Graph.Inductive.Graph(lab,lneighbors,matchAny,labfilter,Node)
import Data.Maybe(catMaybes)

runAutomaton :: Automaton -> String -> [Interval]
runAutomaton automaton string = currentMatches end_state
  where end_state = foldl (runAutomatonOnce automaton) initialState string

data PossibleMatch = P { matchState :: Node, startIndex :: Int }

data ProcessingState = ProcessingState {
    possibleMatches :: [PossibleMatch],
    currentIndex :: Int,
    currentMatches :: [Interval]
}

initialMatch :: PossibleMatch
initialMatch = P { matchState = 0, startIndex = 0 }

initialState :: ProcessingState
initialState = ProcessingState {
    possibleMatches = [initialMatch],
    currentIndex = 0,
    currentMatches = []
}

runAutomatonOnce :: Automaton -> ProcessingState -> Char -> ProcessingState
runAutomatonOnce automaton state char =
    (popFinalStates (finalState automaton)) $
    incrementIndex (runStatesOnce automaton state char)
        -- First get new states by running the automaton
        -- Then pop any final states onto the intervals list

incrementIndex :: ProcessingState -> ProcessingState
incrementIndex state = state { currentIndex = (currentIndex state) + 1 }

-- Runs the automaton over every current possible match, with the given character
runStatesOnce :: Automaton -> ProcessingState -> Char -> ProcessingState
runStatesOnce aut st c = runNonEpsilonMoves aut (runEpsilonMoves aut st) c

-- Takes all the possible matches and multiplies them by the places they can go
-- via only epsilon moves.
runEpsilonMoves :: Automaton -> ProcessingState -> ProcessingState
runEpsilonMoves automaton state = state { possibleMatches = new_possibilities }
  where new_possibilities = concat $ map getEpsilonFriends (possibleMatches state)
        getEpsilonFriends :: PossibleMatch -> [PossibleMatch]
        getEpsilonFriends (P {matchState = s, startIndex = i}) =
            map (\new_state -> P {matchState = new_state, startIndex = i}) $
            findNeighborsOfType (== Epsilon) s (stateMap automaton)

-- Utility for dealing with FGL
-- Filter for nodes with this label, then filter for certain edges out of it,
-- then return all of the labels of the nodes at the end of those edges.
findNeighborsOfType :: (b -> Bool) -> Node -> Gr () b -> [Node]
findNeighborsOfType edge_pred node graph = filtered_neighbors
  where filtered_neighbors = map snd $ filter (\(edge_label,node) -> edge_pred edge_label) neighbors
        neighbors = lneighbors graph node

-- Takes all possible matches and makes them go places based on actual moves
runNonEpsilonMoves :: Automaton -> ProcessingState -> Char -> ProcessingState
runNonEpsilonMoves automaton state char =
    state { possibleMatches = new_possibilities }
    where new_possibilities = concat $ map getMatches (possibleMatches state)
          getMatches :: PossibleMatch -> [PossibleMatch]
          getMatches (P {matchState = s, startIndex = i}) =
              map (\new_state -> P {matchState = new_state, startIndex = i}) $
              findNeighborsOfType (matches char) s (stateMap automaton)

matches :: Char -> Edge -> Bool
matches _ Epsilon = False
matches c (T token) = matchesToken c token

-- Removes all states that are the final state and turns them into matches
popFinalStates :: Node -> ProcessingState -> ProcessingState
popFinalStates final_state state = state { 
    possibleMatches = new_possibilities,
    currentMatches = (currentMatches state) ++ new_matches
} where new_possibilities = filter (not . isEnd) (possibleMatches state)
        new_matches = map toInterval $ filter isEnd (possibleMatches state)

        isEnd :: PossibleMatch -> Bool
        isEnd p = matchState p == final_state
        
        toInterval :: PossibleMatch -> Interval
        toInterval (P {startIndex = i}) = Interval (i, currentIndex state)

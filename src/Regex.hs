module Regex (Regex(..), matchExpression, Interval(..)) where

import Regex.Data
import Regex.Compile
import Regex.Match

import Data.Graph.Inductive.PatriciaTree(Gr(..))
import Data.Graph.Inductive.Graph(lab,lneighbors,matchAny,labfilter,Node)
import Data.Maybe(catMaybes)

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
    -- TODO: Put this into the automaton!!
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
findNeighborsOfType :: Eq a => (b -> Bool) -> a -> Gr a b -> [a]
findNeighborsOfType edge_pred label graph = labels_of_neighbors filtered_neighbors
  where labels_of_neighbors filtered_neighbors = catMaybes $ map (lab graph) filtered_neighbors
        filtered_neighbors = map snd $ filter (\(edge_label,node) -> edge_pred edge_label) neighbors
        neighbors = lneighbors graph node_for_label
        ((_,node_for_label,_,_),empty) = matchAny graph_with_only_this_label
        graph_with_only_this_label = labfilter (==label) graph

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

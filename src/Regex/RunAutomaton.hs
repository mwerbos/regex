module Regex.RunAutomaton where

import Regex.Data
import Regex.Util (findNeighborsOfType,getComponentOfType,collapseSet)
import Regex.Match (matchesToken)

import Data.Graph.Inductive.PatriciaTree(Gr(..))
import Data.Graph.Inductive.Graph(lab,lneighbors,matchAny,labfilter,Node)
import Data.Maybe(catMaybes)
import qualified Data.Set as S
import Data.List (maximumBy,minimumBy,groupBy)

import Debug.Trace (trace) -- TODO remove

-- A final processing step, which takes the matches we've generated
-- (consisting of *all* possible matches) and removes all the matches that don't
-- satisfy the "non-overlapping, greediest possible" criteria.
weedMatches :: [Interval] -> [Interval]
weedMatches =
    map (minimumBy compare_starting_place) . groupBy ending_place_eq .
    map (maximumBy compare_ending_place) . groupBy starting_place_eq
  where starting_place_eq (Interval (a,_)) (Interval (c,_)) = a == c
        ending_place_eq (Interval (_,b)) (Interval (_,d)) = b == d
        compare_starting_place (Interval (a,_)) (Interval (c,_)) = a `compare` c
        compare_ending_place (Interval (_,b)) (Interval (_,d)) = b `compare` d

runAutomaton :: Automaton -> String -> [Interval]
runAutomaton automaton string = S.toList $ currentMatches end_state
  where end_state = foldl (runAutomatonOnce automaton) initialState string

data PossibleMatch = P { matchState :: Node, startIndex :: Int } deriving (Show,Eq,Ord)

data ProcessingState = ProcessingState {
    possibleMatches :: S.Set PossibleMatch,
    currentIndex :: Int,
    currentMatches :: S.Set Interval
} deriving (Show,Eq)

initialMatch :: PossibleMatch
initialMatch = P { matchState = 0, startIndex = 0 }

initialState :: ProcessingState
initialState = ProcessingState {
    possibleMatches = S.insert initialMatch S.empty,
    currentIndex = 0,
    currentMatches = S.empty
}

runAutomatonOnce :: Automaton -> ProcessingState -> Char -> ProcessingState
runAutomatonOnce automaton state char =
    addInitialState $
    (popFinalStates (finalState automaton)) $
    incrementIndex (runStatesOnce automaton char state)
        -- First get new states by running the automaton
        -- Then pop any final states onto the intervals list

addInitialState :: ProcessingState -> ProcessingState
addInitialState state = state {
  possibleMatches = S.insert (P { matchState = 0, startIndex = currentIndex state })
                             (possibleMatches state)
}

incrementIndex :: ProcessingState -> ProcessingState
incrementIndex state = state { currentIndex = (currentIndex state) + 1 }

-- Runs the automaton over every current possible match, with the given character
runStatesOnce :: Automaton -> Char -> ProcessingState -> ProcessingState
runStatesOnce aut c st = (runEpsilonMoves aut . runNonEpsilonMoves aut c . runEpsilonMoves aut) st

-- Takes all the possible matches and multiplies them by the places they can go
-- via only epsilon moves.
runEpsilonMoves :: Automaton -> ProcessingState -> ProcessingState
runEpsilonMoves automaton state = state { possibleMatches = new_possibilities }
  where new_possibilities = S.union
            (possibleMatches state) (collapseSet (S.map getEpsilonFriends (possibleMatches state)))
        getEpsilonFriends :: PossibleMatch -> S.Set PossibleMatch
        getEpsilonFriends (P {matchState = s, startIndex = i}) =
            S.map (\new_state -> P {matchState = new_state, startIndex = i}) $
            getComponentOfType (== Epsilon) s (stateMap automaton)

-- Takes all possible matches and makes them go places based on actual moves
runNonEpsilonMoves :: Automaton -> Char -> ProcessingState -> ProcessingState
runNonEpsilonMoves automaton char state =
    state { possibleMatches = new_possibilities }
    where new_possibilities = collapseSet $ S.map getMatches (possibleMatches state)
          getMatches :: PossibleMatch -> S.Set PossibleMatch
          getMatches (P {matchState = s, startIndex = i}) =
              S.map (\new_state -> P {matchState = new_state, startIndex = i}) $
              findNeighborsOfType (matches char) (stateMap automaton) s 

matches :: Char -> Edge -> Bool
matches _ Epsilon = False
matches c (T token) = matchesToken c token

-- Removes all states that are the final state and turns them into matches
popFinalStates :: Node -> ProcessingState -> ProcessingState
popFinalStates final_state state = state { 
    possibleMatches = new_possibilities,
    currentMatches = S.union (currentMatches state) new_matches
} where new_possibilities = S.filter (not . isEnd) (possibleMatches state)
        new_matches = S.map toInterval $ S.filter isEnd (possibleMatches state)

        isEnd :: PossibleMatch -> Bool
        isEnd p = matchState p == final_state
        
        toInterval :: PossibleMatch -> Interval
        toInterval (P {startIndex = i}) = Interval (i, currentIndex state)

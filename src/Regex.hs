module Regex (Regex(..), matchExpression, Interval(..)) where

import Regex.Data
import Regex.Preprocess

matchExpression :: Regex -> String -> [Interval]
matchExpression regex string = runAutomaton automaton string
  where automaton = processRegex regex

runAutomaton :: Automaton -> String -> [Interval]
runAutomaton automaton string = endIntervals
  where (endStates, endIntervals) = foldl (runAutomatonOnce automaton) ([S 0],[]) string

runAutomatonOnce :: Automaton -> ([State],[Interval]) -> Char -> ([State],[Interval])
runAutomatonOnce automaton (states, intervals) char = (popped_states, popped_intervals)
        -- First get new states by running the automaton
  where new_states = runStatesOnce automaton states char
        -- Then pop any final states onto the intervals list
        (popped_states,popped_intervals) = popFinalStates new_states intervals
        

popFinalStates :: [State] -> [Interval] -> ([State], [Interval])
popFinalStates = error "popFinalStates undefined"

runStatesOnce :: Automaton -> [State] -> Char -> [State]
runStatesOnce = error "runStatesOnce undefined" 

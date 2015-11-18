module Regex (Regex(..), matchExpression, Interval(..)) where

import Regex.Data
import Regex.RunAutomaton (runAutomaton)
import Regex.Compile (processRegex)

import Debug.Trace (trace) -- TODO remove

matchExpression :: Regex -> String -> [Interval]
matchExpression (Regex r) string = 
    trace ("matching on string: '" ++ string ++ "' with regex '" ++ r ++ "'") $
    trace ("got automaton: " ++ (show automaton)) $
    runAutomaton automaton string
  where automaton = processRegex (Regex r) 

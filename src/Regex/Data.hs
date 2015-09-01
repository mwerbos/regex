module Regex.Data where

import Data.Graph.Inductive.PatriciaTree

data Regex = Regex String deriving (Show, Eq)

-- An interval. (x,y) => y must > x
data Interval = Interval (Int, Int) deriving (Show, Eq)

-- Just a wrapped int
data State = S Int deriving (Show, Eq, Ord)

-- An edge in the graph can be a token or an epsilon-transition
-- (a transition requiring 0 characters to match)
data Edge = T Token | Epsilon deriving (Show, Eq)

-- A regex token
data Token =
    Single Char |
    Group [Token] |
    NegGroup [Token] |
    Or Token Token |
    Repeated Token |
    Wildcard
  deriving (Show, Eq)

-- A nondeterministic finite automaton
data Automaton = Automaton {
  stateMap :: Gr State Edge, -- Graph with nodes that are states
  finalState :: Node
}

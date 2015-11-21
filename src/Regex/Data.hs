module Regex.Data where

import Data.Graph.Inductive.PatriciaTree(Gr(..))
import Data.Graph.Inductive.Graph(Node)

data Regex = Regex String deriving (Show, Eq)

-- An interval. (x,y) => y must > x
data Interval = Interval (Int, Int) deriving (Show, Eq, Ord)

-- An edge in the graph can be a token or an epsilon-transition
-- (a transition requiring 0 characters to match)
data Edge = T Token | Epsilon deriving (Show, Eq, Ord)

-- A regex token
data Token =
    Single Char |
    NegChar Char |
    Group [Token] |
    NoneOf [Token] |
    Or Token Token |
    Repeated Token |
    Wildcard
  deriving (Show, Eq, Ord)

-- A nondeterministic finite automaton
data Automaton = Automaton {
  stateMap :: Gr () Edge, -- Graph with edges that are tokens
  finalState :: Node
} deriving (Eq,Show)

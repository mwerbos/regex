module Regex.Data where

import Data.Graph.Inductive.PatriciaTree(Gr(..))
import Data.Graph.Inductive.Graph(Node)

data Regex = Regex String deriving (Show, Eq)

-- An interval. (x,y) => y must > x
data Interval = Interval (Int, Int) deriving (Show, Eq, Ord)

-- An edge in the graph can be a simple token or an epsilon-transition
-- (a transition requiring 0 characters to match)
data Edge = T SimpleToken | Epsilon deriving (Show, Eq, Ord)

-- Tokens that have to be represented as primitives for matching.
data SimpleToken =
    Single Char |
    NegChar Char |
    NoneOf [Char] |
    Wildcard
  deriving (Show, Eq, Ord)

-- A regex token
data Token =
    Simple SimpleToken |
    Group [SimpleToken] |
    Or [Token] [[Token]] |
    Repeated Token
  deriving (Show, Eq, Ord)

-- A nondeterministic finite automaton
data Automaton = Automaton {
  stateMap :: Gr () Edge, -- Graph with edges that are tokens
  finalState :: Node
} deriving (Eq,Show)

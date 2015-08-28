module Regex (Regex(..), matchExpression, Interval(..)) where

data Regex = Regex String deriving (Show, Eq)

-- An interval. (x,y) => y must > x
data Interval = Interval (Int, Int) deriving (Show, Eq)

matchExpression :: Regex -> String -> [Interval]
matchExpression = undefined

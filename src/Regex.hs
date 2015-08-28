module Regex (Regex(..), matchExpression, Interval(..)) where

data Regex = Regex String

-- An interval. (x,y) => y must > x
data Interval = (Int, Int)

matchExpression :: Regex -> String -> [Interval]
matchExpression = undefined

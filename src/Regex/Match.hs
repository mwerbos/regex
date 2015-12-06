module Regex.Match where

import Regex.Data

matchesToken :: Char -> Token -> Bool
matchesToken c (Single token_char) = c == token_char
matchesToken c (NegChar token_char) = c /= token_char
matchesToken c Wildcard = True
matchesToken c (NoneOf tokens) = all (not . matchesToken c) tokens
matchesToken _ t = error ("matchesToken undefined for this token: " ++ show t)

module Regex.Match where

import Regex.Data

matchesToken :: Char -> Token -> Bool
matchesToken c (Single token_char) = c == token_char
matchesToken c (NegChar token_char) = c /= token_char
matchesToken _ t = error ("matchesToken undefined for this token: " ++ show t)

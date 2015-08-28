module Regex.Preprocess where

import Regex.Data

data TokenType =
    Backslash |
    LBracket |
    RBracket |
    Dot |
    Carat |
    OtherChar

-- Token is here a token within a Regex string,
-- not a regular expression token matcher.
tokenType :: Char -> TokenType
tokenType '\\' = Backslash
tokenType '[' = LBracket
tokenType ']' = RBracket
tokenType '.' = Dot 
tokenType '^' = Carat
tokenType _ = OtherChar

type TokenizedRegex = [(Char, TokenType)]

-- List is of *regular expression* tokens
type ParsedRegex = [Token]

-- Pre-processes regex into a nondeterministic finite state automaton
processRegex :: Regex -> Automaton
processRegex = makeAutomaton . parse . tokenize

parse :: TokenizedRegex -> ParsedRegex
parse = error "parse is undefined"

tokenize :: Regex -> TokenizedRegex
tokenize = error "tokenize is undefined"

makeAutomaton :: ParsedRegex -> Automaton
makeAutomaton = error "makeAutomaton is undefined"

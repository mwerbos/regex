module Regex.Compile where

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

data MaybeParsed = Bare (Char, TokenType) | Parsed Token

type PartiallyParsedRegex = [MaybeParsed]

-- Pre-processes regex into a nondeterministic finite state automaton
processRegex :: Regex -> Automaton
processRegex = makeAutomaton . parse . tokenize

tokenize :: Regex -> TokenizedRegex
tokenize (Regex str) = map (\x -> (x, tokenType x)) str

makeAutomaton :: ParsedRegex -> Automaton
makeAutomaton = error "makeAutomaton is undefined"

parse :: TokenizedRegex -> ParsedRegex
parse = forceParsed . parseNegations . parseGroups . parseEscapes . makeMaybeParsed

forceParsed :: PartiallyParsedRegex -> ParsedRegex
forceParsed (Bare (c,t):rest) = error $ "Could not parse char: " ++ [c]
forceParsed (Parsed t:rest) = t:forceParsed rest
forceParsed [] = []

makeMaybeParsed :: TokenizedRegex -> PartiallyParsedRegex
makeMaybeParsed = map Bare

parseEscapes :: PartiallyParsedRegex -> PartiallyParsedRegex
parseEscapes (Bare (_, Backslash):Bare (c, OtherChar):rest) = Parsed (Single c) : parseEscapes rest
parseEscapes (Bare (_, Backslash):Bare (c,_):rest) = error ("Character '" ++ [c] ++ "' may not be escaped")
-- This next case shouldn't happen since we parse escapes first
parseEscapes (Bare (_, Backslash):_:rest) = error "Can't escape token"
parseEscapes (_:rest) = parseEscapes rest
parseEscapes [] = []

parseGroups :: PartiallyParsedRegex -> PartiallyParsedRegex
parseGroups = error "parseGroups undefined"

parseNegations :: PartiallyParsedRegex -> PartiallyParsedRegex
parseNegations = error "parseNegations undefined"
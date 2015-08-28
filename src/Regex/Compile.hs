module Regex.Compile where

import Regex.Data
import Data.Graph.Inductive(empty,Gr(..))

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
makeAutomaton tokens = Automaton { stateMap = foldl addToken empty tokens }

addToken :: Gr State Edge -> Token -> Gr State Edge
addToken = error "addToken undefined"

parse :: TokenizedRegex -> ParsedRegex
parse =
    forceParsed .
    parseLeftovers .
    parseWildcards .
    parseNegations .
    parseGroups .
    parseEscapes .
    makeMaybeParsed

forceParsed :: PartiallyParsedRegex -> ParsedRegex
forceParsed (Bare (c,t):rest) = error $ "Could not parse char: " ++ [c]
forceParsed (Parsed t:rest) = t:forceParsed rest
forceParsed [] = []

makeMaybeParsed :: TokenizedRegex -> PartiallyParsedRegex
makeMaybeParsed = map Bare

-- TODO: Refactor to make the parsing logic a bit less ugly
parseEscapes :: PartiallyParsedRegex -> PartiallyParsedRegex
parseEscapes (Bare (_, Backslash):Bare (c, OtherChar):rest) =
    error ("Character '" ++ [c] ++ "' may not be escaped")
parseEscapes (Bare (_, Backslash):Bare (c,_):rest) = Parsed (Single c) : parseEscapes rest
-- This next case shouldn't happen since we parse escapes first
parseEscapes (Bare (_, Backslash):_:rest) = error "Can't escape token"
parseEscapes (_:rest) = parseEscapes rest
parseEscapes [] = []

parseGroups :: PartiallyParsedRegex -> PartiallyParsedRegex
parseGroups = error "parseGroups undefined"

parseNegations :: PartiallyParsedRegex -> PartiallyParsedRegex
parseNegations = error "parseNegations undefined"

parseWildcards :: PartiallyParsedRegex -> PartiallyParsedRegex
parseWildcards = error "parseWildcards undefined"

parseLeftovers :: PartiallyParsedRegex -> PartiallyParsedRegex
parseLeftovers = map convertChars
  where convertChars :: MaybeParsed -> MaybeParsed
        convertChars (Bare (c, OtherChar)) = Parsed (Single c)
        convertChars (Parsed token) = Parsed token
        convertChars _ = error "parse error when running parseLeftovers"

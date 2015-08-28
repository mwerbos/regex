module Regex.Compile where

import Regex.Data
import Data.Graph.Inductive(empty,Gr(..))

data TokenType =
    Backslash |
    LBracket |
    RBracket |
    Dot |
    Carat |
    Star |
    OtherChar

-- Token is here a token within a Regex string,
-- not a regular expression token matcher.
tokenType :: Char -> TokenType
tokenType '\\' = Backslash
tokenType '[' = LBracket
tokenType ']' = RBracket
tokenType '.' = Dot 
tokenType '^' = Carat
tokenType '*' = Star
tokenType _ = OtherChar

type TokenizedRegex = [(Char, TokenType)]

-- List is of *regular expression* tokens
type ParsedRegex = [Token]

-- If I were really hardcore, each parsing stage would have its own type.
-- But I'm not.
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
    parseRepeats .
    parseNegations .
    parseGroups .
    parseLeftovers .
    parseWildcards .
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

parseRepeats :: PartiallyParsedRegex -> PartiallyParsedRegex
parseRepeats (Bare (c,Star):Parsed token:rest) = (Parsed (Repeated token)):parseRepeats rest
parseRepeats (Bare (c,Star):_:rest) = error "Unexpected * after unparsed input"
parseRepeats (Bare (c,Star):[]) = error "Unexpected * at beginning of input"
parseRepeats (other:rest) = other:parseRepeats rest
parseRepeats [] = []

parseNegations :: PartiallyParsedRegex -> PartiallyParsedRegex
parseNegations = error "parseNegations undefined"

parseWildcards :: PartiallyParsedRegex -> PartiallyParsedRegex
parseWildcards = map convertDots
    where convertDots :: MaybeParsed -> MaybeParsed
          convertDots (Bare ('.',Dot)) = Parsed Wildcard
          convertDots other = other

parseLeftovers :: PartiallyParsedRegex -> PartiallyParsedRegex
parseLeftovers = map convertChars
  where convertChars :: MaybeParsed -> MaybeParsed
        convertChars (Bare (c, OtherChar)) = Parsed (Single c)
        -- Let brackets, negations, and repeats pass through unchanged
        convertChars (Bare (c, LBracket)) = Bare (c, LBracket)
        convertChars (Bare (c, RBracket)) = Bare (c, RBracket)
        convertChars (Bare (c, Carat)) = Bare (c, Carat)
        convertChars (Bare (c, Star)) = Bare (c, Star)
        -- Let parsed tokens pass through unchanged
        convertChars (Parsed token) = Parsed token
        -- Expect not to see any other type of character (not strictly necessary)
        convertChars _ = error "parse error when running parseLeftovers"

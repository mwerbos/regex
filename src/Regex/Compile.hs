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
  deriving Eq

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

data IsNegated = Negated | Unnegated deriving Eq

-- If I were really hardcore, each parsing stage would have its own type.
-- But I'm not.
data MaybeParsed =
    Bare (Char, TokenType) |
    Parsed Token |
    -- Should not contain recursive PartiallyParsedGroups.
    -- TODO: Enforce this somehow? By adding another wrapper?
    PartiallyParsedGroup [MaybeParsed] IsNegated
  deriving Eq

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
parseGroups tokens = map (innerParseGroup . maybeNegate) (makeGroupPiles tokens)

-- For each PartiallyParsed Group, if it starts with a carat,
-- negates the group.
maybeNegate :: MaybeParsed -> MaybeParsed
maybeNegate (PartiallyParsedGroup ((Bare (_, Carat)):rest) Unnegated) =
  PartiallyParsedGroup rest Negated
maybeNegate other = other

-- Looks for [ and ] and just puts everything between them in a PartiallyParsedGroup.

data LatestGroup = NoGroup | IncompleteGroup [MaybeParsed] deriving Eq
makeGroupPiles :: PartiallyParsedRegex -> PartiallyParsedRegex
makeGroupPiles tokens = final_parse
  where final_parse = if last_group == NoGroup
                      then parsed_regex
                      else error "Expected ] before end of regex"
        (last_group, parsed_regex) = foldl grab_chars (NoGroup,[]) tokens
        grab_chars :: (LatestGroup,PartiallyParsedRegex) -> 
                          MaybeParsed ->
                              (LatestGroup,PartiallyParsedRegex)
        grab_chars (NoGroup,parsed) (Bare (_,LBracket)) = (IncompleteGroup [],parsed)
        grab_chars (_,parsed) (Bare (_,LBracket)) = error "Encountered [ inside group"
        grab_chars (NoGroup,_) (Bare (_,RBracket)) = error "Encountered ] outside group"
        grab_chars (IncompleteGroup group, parsed) (Bare (_,RBracket)) =
            (NoGroup, parsed ++ [PartiallyParsedGroup group Unnegated])
        grab_chars (IncompleteGroup group, parsed) next_token =
            (IncompleteGroup (group ++ [next_token]), parsed)
        grab_chars (NoGroup, parsed) next_token = (NoGroup, parsed ++ [next_token])

-- If it's a group, parses the characters inside the group.
-- Only wildcards and escapes are allowed.
innerParseGroup :: MaybeParsed -> MaybeParsed
innerParseGroup (PartiallyParsedGroup token_tuples Negated) =
    Parsed (Group 
        (forceParsed $ parseLeftovers $ parseWildcards $ parseEscapes token_tuples))
innerParseGroup (PartiallyParsedGroup token_tuples Unnegated) =
    Parsed (NegGroup
        (forceParsed $ parseLeftovers $ parseWildcards $ parseEscapes token_tuples))
innerParseGroup other = other


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

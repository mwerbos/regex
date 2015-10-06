module Regex.Compile where

import Regex.Data
import Data.Graph.Inductive(empty,Gr(..),insNodes,insEdge)
import Debug.Trace (trace) -- TODO remove

data TokenType =
    Backslash |
    LBracket |
    RBracket |
    Dot |
    Carat |
    Star |
    Plus |
    OtherChar
  deriving (Eq,Show)

-- Token is here a token within a Regex string,
-- not a regular expression token matcher.
tokenType :: Char -> TokenType
tokenType '\\' = Backslash
tokenType '[' = LBracket
tokenType ']' = RBracket
tokenType '.' = Dot 
tokenType '^' = Carat
tokenType '*' = Star
tokenType '+' = Plus 
tokenType _ = OtherChar

type TokenizedRegex = [(Char, TokenType)]

-- List is of *regular expression* tokens
type ParsedRegex = [Token]

data IsNegated = Negated | Unnegated deriving (Eq,Show)

-- If I were really hardcore, each parsing stage would have its own type.
-- But I'm not.
data MaybeParsed =
    Bare (Char, TokenType) |
    Parsed Token |
    -- Should not contain recursive PartiallyParsedGroups.
    -- TODO: Enforce this somehow? By adding another wrapper?
    PartiallyParsedGroup [MaybeParsed] IsNegated
  deriving (Eq,Show)

type PartiallyParsedRegex = [MaybeParsed]

-- Pre-processes regex into a nondeterministic finite state automaton
processRegex :: Regex -> Automaton
processRegex regex =
    trace ("parsed regex: " ++ show (parse $ tokenize regex)) $
    trace ("tokenized regex: " ++ show (tokenize regex)) $
    (makeAutomaton . parse . tokenize) regex

tokenize :: Regex -> TokenizedRegex
tokenize (Regex str) = map (\x -> (x, tokenType x)) str

makeAutomaton :: ParsedRegex -> Automaton
makeAutomaton tokens = foldl addToken emptyAutomaton tokens

-- TODO gracefully handle the empty case
emptyAutomaton :: Automaton
emptyAutomaton = Automaton {
    stateMap = empty,
    finalState = -1
}

addToken :: Automaton -> Token -> Automaton
addToken automaton token
  | automaton == emptyAutomaton = makeMiniAutomaton token
  | otherwise = addMiniAutomaton (makeMiniAutomaton token) automaton

addMiniAutomaton :: Automaton -> Automaton -> Automaton
addMiniAutomaton mini_graph graph = error "addMiniAutomaton undefined"

-- Combines two regexes saying you can see either one of them
orAutomatons :: Automaton -> Automaton -> Automaton
orAutomatons = error "orAutomatons is undefined"

makeMiniAutomaton :: Token -> Automaton
makeMiniAutomaton (Single c) = Automaton {
  stateMap = insEdge (0, 1, T $ Single c) $ insNodes [(0, S 0), (1, S 1)] $ empty,
  finalState = 1
}
makeMiniAutomaton (Or t1 t2) = orAutomatons (makeMiniAutomaton t1) (makeMiniAutomaton t2)
makeMiniAutomaton (Group tokens) = foldl orAutomatons emptyAutomaton $ map makeMiniAutomaton tokens
makeMiniAutomaton _ = error "makeMiniAutomaton undefined for this token type"

parse :: TokenizedRegex -> ParsedRegex
parse regex =
   (forceParsed .
    parseRepeats .
    parseGroups .
    parseLeftovers .
    parseWildcards .
    parseEscapes .
    makeMaybeParsed)
    regex

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
parseEscapes (thing:rest) = thing:parseEscapes rest
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
parseRepeats (Bare (c,Plus):Parsed token:rest) = (Parsed (Repeated token)):Parsed token:parseRepeats rest
parseRepeats (Bare (c,Star):_:rest) = error "Unexpected * after unparsed input"
parseRepeats (Bare (c,Plus):_:rest) = error "Unexpected + after unparsed input"
parseRepeats (Bare (c,Star):[]) = error "Unexpected * at beginning of input"
parseRepeats (Bare (c,Plus):[]) = error "Unexpected + at beginning of input"
parseRepeats (other:rest) = other:parseRepeats rest
parseRepeats [] = []

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
        convertChars (Bare (c, Plus)) = Bare (c, Plus)
        -- Let parsed tokens pass through unchanged
        convertChars (Parsed token) = Parsed token
        -- Expect not to see any other type of character (not strictly necessary)
        convertChars other = error ("parse error when running parseLeftovers: " ++ show other)

module Regex.Compile where

import Regex.Data
import Regex.Util
import Data.Graph.Inductive(empty,Gr,insNodes,insEdge,insEdges,mkGraph,Node)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)

import Debug.Trace (trace) -- TODO remove

data TokenType =
    Backslash | LBracket |
    RBracket |
    Dot |
    Carat |
    Star |
    Plus |
    LParen | RParen | Pipe |
    OtherChar
  deriving (Eq,Show)

-- Token is here a token within a Regex string,
-- not a regular expression token matcher.
tokenType :: Char -> TokenType
tokenType '\\' = Backslash
tokenType '[' = LBracket
tokenType ']' = RBracket
tokenType '(' = LParen
tokenType ')' = RParen
tokenType '.' = Dot 
tokenType '^' = Carat
tokenType '*' = Star
tokenType '+' = Plus 
tokenType '(' = LParen
tokenType ')' = RParen
tokenType '|' = Pipe
tokenType _ = OtherChar

type TokenizedRegex = [(Char, TokenType)]

-- List is of *regular expression* tokens
type ParsedRegex = [Token]

data IsNegated = Negated | Unnegated deriving (Eq,Show)

-- If I were really hardcore, each parsing stage would have its own type.
-- But I'm not.
data MaybeParsed =
    Unparsed (Char, TokenType) |
    Parsed Token |
    -- Should not contain recursive PartiallyParsedCharacterClasses.
    -- TODO: Enforce this somehow? By adding another wrapper?
    PartiallyParsedCharacterClass [MaybeParsed] IsNegated |
    -- Should not contain recursive PartiallyParsedGroups.
    -- Has the first regex, and then all other regexes (optional)
    PartiallyParsedGroup [MaybeParsed] [[MaybeParsed]]
  deriving (Eq,Show)

type PartiallyParsedRegex = [MaybeParsed]

-- Pre-processes regex into a nondeterministic finite state automaton
processRegex :: Regex -> Automaton
processRegex regex =
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

-- Just concatenate the regexes; we want to see 'graph' then 'mini_graph'
addMiniAutomaton :: Automaton -> Automaton -> Automaton
addMiniAutomaton mini_graph graph = Automaton {
    stateMap = 
      insEdge (finalState graph,  find_updated_node 0, Epsilon) combined_graph,
      finalState = find_updated_node (finalState mini_graph)
    }
  where find_updated_node state = 
            let maybe_new_node = M.lookup state new_node_map in
            if isJust maybe_new_node then fromJust maybe_new_node
            else error ("Could not find state " ++ show state ++ " in map " ++ show new_node_map)

        (combined_graph, new_node_map) = 
            addGraphsAndTranslate (stateMap graph) (stateMap mini_graph)

-- Combines two regexes saying you can see either one of them
orAutomatons :: Automaton -> Automaton -> Automaton
orAutomatons first_aut second_aut 
  | first_aut == emptyAutomaton = second_aut
  | second_aut == emptyAutomaton = first_aut
  | otherwise = 
      Automaton {
        stateMap = 
            add_epsilon_transitions overall_graph,
        finalState = translate_base_graph_node 1
      }
  where (overall_graph,
            (translate_base_graph_node,
             translate_first_graph_node,
             translate_second_graph_node)) =
            combineThreeGraphs (base_graph, stateMap first_aut, stateMap second_aut)
        -- base_graph: just the beginning and end nodes we add to A and B
        base_graph = mkGraph [(0,()), (1,())] []
        -- No translation needed for nodes from the base graph
        add_epsilon_transitions overall_graph = 
            insEdges epsilon_edges overall_graph
        epsilon_edges = [(translate_base_graph_node 0, translate_first_graph_node 0, Epsilon),
                         (translate_base_graph_node 0, translate_second_graph_node 0, Epsilon),
                         (translate_first_graph_node $ finalState first_aut,
                              translate_base_graph_node 1, Epsilon),
                         (translate_second_graph_node $ finalState second_aut,
                              translate_base_graph_node 1, Epsilon)]

-- Combines three graphs and returns a translation function from each graph to the combined graph.
combineThreeGraphs :: (Show a, Show b) =>
    (Gr a b, Gr a b, Gr a b) -> (Gr a b, (Node -> Node, Node -> Node, Node -> Node))
combineThreeGraphs (one, two, three) =
    (overall_graph, (translate_first_node, translate_second_node, translate_third_node))
  where (overall_graph, translated_two_and_three) = addGraphsAndTranslate one two_and_three
        (two_and_three, third_graph_translated) = addGraphsAndTranslate two three
        translate_first_node node = node 
        translate_second_node node = M.findWithDefault node node translated_two_and_three
        translate_third_node node = 
          let first_translation = M.findWithDefault node node third_graph_translated in
          M.findWithDefault first_translation first_translation translated_two_and_three
        

makeMiniAutomaton :: Token -> Automaton
makeMiniAutomaton (Single c) = Automaton {
  stateMap = insEdge (0, 1, T $ Single c) $ insNodes [(0,()), (1,())] $ empty,
  finalState = 1
}
makeMiniAutomaton (NegChar c) = Automaton {
  stateMap = insEdge (0, 1, T $ NegChar c) $ insNodes [(0,()), (1,())] $ empty,
  finalState = 1
}
makeMiniAutomaton (CharacterClass tokens) = 
    foldl orAutomatons emptyAutomaton $ map makeMiniAutomaton tokens
makeMiniAutomaton (NoneOf tokens) = Automaton {
  stateMap = insEdge (0, 1, T $ NoneOf tokens) $ insNodes [(0,()), (1,())] $ empty,
  finalState = 1
}
makeMiniAutomaton (Or regex1 regex2s) =
    orAutomatons (makeAutomaton regex1)
        (foldl orAutomatons emptyAutomaton $ map makeAutomaton regex2s)
makeMiniAutomaton (Repeated token) = connect_ends $ makeMiniAutomaton token
  where connect_ends automaton = automaton {
            stateMap = insEdge (finalState automaton, 0, Epsilon) $
                       insEdge (0, finalState automaton, Epsilon) $
                       (stateMap automaton)
        }
-- TODO: Think about having a smaller "SimpleToken" type that encompasses *just*
-- the token types that can be placed on edges (Char, NegChar, Wildcard),
-- and think about how Wildcard and NegChar affect matchesToken and whether I need to think
-- of them as match *subsets*.
makeMiniAutomaton Wildcard = Automaton {
  stateMap = insEdge (0, 1, T $ Wildcard) $ insNodes [(0,()), (1,())] $ empty,
  finalState = 1
}

parse :: TokenizedRegex -> ParsedRegex
parse regex =
   (forceParsed .
    parseRepeats .
    parseGroups .
    parseCharacterClasses .
    parseLeftovers .
    parseWildcards .
    parseEscapes .
    makeMaybeParsed)
    regex

forceParsed :: PartiallyParsedRegex -> ParsedRegex
forceParsed (Unparsed (c,t):rest) = error $ "Could not parse char: " ++ [c]
forceParsed (Parsed t:rest) = t:forceParsed rest
forceParsed [] = []

makeMaybeParsed :: TokenizedRegex -> PartiallyParsedRegex
makeMaybeParsed = map Unparsed

-- TODO: Refactor to make the parsing logic a bit less ugly
parseEscapes :: PartiallyParsedRegex -> PartiallyParsedRegex
parseEscapes (Unparsed (_, Backslash):Unparsed (c, OtherChar):rest) =
    error ("Character '" ++ [c] ++ "' may not be escaped")
parseEscapes (Unparsed (_, Backslash):Unparsed (c,_):rest) = Parsed (Single c) : parseEscapes rest
-- This next case shouldn't happen since we parse escapes first
parseEscapes (Unparsed (_, Backslash):_:rest) = error "Can't escape token"
parseEscapes (thing:rest) = thing:parseEscapes rest
parseEscapes [] = []

parseGroups :: PartiallyParsedRegex -> PartiallyParsedRegex
parseGroups tokens = map innerParseGroup (makeGroupPiles tokens)

data LatestGroup =
    NoGroup |
    GroupBeforeFirstElem [MaybeParsed] |
    GroupAfterFirstElem [MaybeParsed] [[MaybeParsed]]
  deriving Eq
makeGroupPiles :: PartiallyParsedRegex -> PartiallyParsedRegex
makeGroupPiles tokens = final_parse
  where final_parse = if last_group == NoGroup
                      then parsed_regex
                      else error "Expected ) before end of regex"
        (last_group, parsed_regex) = foldl grab_tokens (NoGroup,[]) tokens
        grab_tokens :: (LatestGroup,PartiallyParsedRegex)
                           -> MaybeParsed -> (LatestGroup,PartiallyParsedRegex)

        grab_tokens (NoGroup,parsed) (Unparsed (_,LParen)) = (GroupBeforeFirstElem [],parsed)
        grab_tokens (_,parsed) (Unparsed (_,LParen)) = error "Encountered ( inside group"
        grab_tokens (NoGroup,_) (Unparsed (_,RParen)) = error "Encountered ) outside group"
        grab_tokens (NoGroup,_) (Unparsed (_,Pipe)) = error "Encountered ) outside group"
        grab_tokens (NoGroup, parsed) other_token = (NoGroup, parsed ++ [other_token])

        grab_tokens (GroupBeforeFirstElem tokens, parsed) (Unparsed (_,Pipe)) =
            (GroupAfterFirstElem tokens [], parsed)
        grab_tokens (GroupBeforeFirstElem tokens,parsed) (Unparsed (_,RParen)) =
            (NoGroup, parsed ++ [PartiallyParsedGroup tokens []])
        grab_tokens (GroupBeforeFirstElem tokens, parsed) other_token =
            (GroupBeforeFirstElem (tokens ++ [other_token]), parsed)

        grab_tokens (GroupAfterFirstElem regex regexes, parsed) (Unparsed (_,Pipe)) =
            (GroupAfterFirstElem regex (regexes ++ [[]]), parsed)
        grab_tokens (GroupAfterFirstElem regex regexes, parsed) (Unparsed (_,RParen)) =
            (NoGroup, parsed ++ [PartiallyParsedGroup regex regexes])
        grab_tokens (GroupAfterFirstElem regex regexes, parsed) other_token =
            (GroupAfterFirstElem regex (add_to_last other_token regexes), parsed)

        add_to_last :: a -> [[a]] -> [[a]]
        add_to_last x xss = all_but_last xss ++ [last xss ++ [x]]
        all_but_last :: [a] -> [a]
        all_but_last (x:y:xs) = x:(all_but_last (y:xs))
        all_but_last [x] = []
        all_but_last [] = error "encountered group section with no elements"

parseCharacterClasses :: PartiallyParsedRegex -> PartiallyParsedRegex
parseCharacterClasses tokens = map (innerParseCharacterClass . maybeNegate) (makeCharacterClassPiles tokens)

-- For each PartiallyParsed CharacterClass, if it starts with a carat,
-- negates the group.
maybeNegate :: MaybeParsed -> MaybeParsed
maybeNegate (PartiallyParsedCharacterClass ((Unparsed (_, Carat)):rest) Unnegated) =
  PartiallyParsedCharacterClass rest Negated
maybeNegate other = other

-- Looks for [ and ] and just puts everything between them in a PartiallyParsedCharacterClass.

data LatestCharacterClass = NoCharacterClass | IncompleteCharacterClass [MaybeParsed] deriving Eq
makeCharacterClassPiles :: PartiallyParsedRegex -> PartiallyParsedRegex
makeCharacterClassPiles tokens = final_parse
  where final_parse = if last_group == NoCharacterClass
                      then parsed_regex
                      else error "Expected ] before end of regex"
        (last_group, parsed_regex) = foldl grab_chars (NoCharacterClass,[]) tokens
        grab_chars :: (LatestCharacterClass,PartiallyParsedRegex) -> 
                          MaybeParsed ->
                              (LatestCharacterClass,PartiallyParsedRegex)
        grab_chars (NoCharacterClass,parsed) (Unparsed (_,LBracket)) = (IncompleteCharacterClass [],parsed)
        grab_chars (_,parsed) (Unparsed (_,LBracket)) = error "Encountered [ inside character class"
        grab_chars (NoCharacterClass,_) (Unparsed (_,RBracket)) = error "Encountered ] outside character class"
        grab_chars (IncompleteCharacterClass group, parsed) (Unparsed (_,RBracket)) =
            (NoCharacterClass, parsed ++ [PartiallyParsedCharacterClass group Unnegated])
        grab_chars (IncompleteCharacterClass group, parsed) next_token =
            (IncompleteCharacterClass (group ++ [next_token]), parsed)
        grab_chars (NoCharacterClass, parsed) next_token = (NoCharacterClass, parsed ++ [next_token])

innerParseGroup :: MaybeParsed -> MaybeParsed
innerParseGroup (PartiallyParsedGroup regex regexes) =
    Parsed (Or (parse_inner regex) (map parse_inner regexes))
  where parse_inner :: PartiallyParsedRegex -> ParsedRegex
        parse_inner = forceParsed . parseLeftovers . parseWildcards . parseEscapes
innerParseGroup other = other

-- If it's a character class, parses the characters inside the character class.
-- Only wildcards and escapes are allowed.
innerParseCharacterClass :: MaybeParsed -> MaybeParsed
innerParseCharacterClass (PartiallyParsedCharacterClass token_tuples Unnegated) =
    Parsed (CharacterClass 
        (forceParsed $ parseLeftovers $ parseWildcards $ parseEscapes token_tuples))
innerParseCharacterClass (PartiallyParsedCharacterClass token_tuples Negated) =
    Parsed (NoneOf
        (forceParsed $ parseLeftovers $ parseEscapes token_tuples))
innerParseCharacterClass other = other


parseRepeats :: PartiallyParsedRegex -> PartiallyParsedRegex
parseRepeats (Parsed token:Unparsed (c,Star):rest) =
    (Parsed (Repeated token)):parseRepeats rest
parseRepeats (Parsed token:Unparsed (c,Plus):rest) =
    Parsed token:(Parsed (Repeated token)):parseRepeats rest
parseRepeats (_:Unparsed (c,Star):rest) = error "Unexpected *"
parseRepeats (_:Unparsed (c,Plus):rest) = error "Unexpected +"
parseRepeats (Unparsed (c,Star):rest) = error "Unexpected *"
parseRepeats (Unparsed (c,Plus):rest) = error "Unexpected +"
parseRepeats (other:rest) = other:parseRepeats rest
parseRepeats [] = trace ("Parsing repeats from empty list") []

parseWildcards :: PartiallyParsedRegex -> PartiallyParsedRegex
parseWildcards = map convertDots
    where convertDots :: MaybeParsed -> MaybeParsed
          convertDots (Unparsed ('.',Dot)) = Parsed (Wildcard)
          convertDots other = other

parseLeftovers :: PartiallyParsedRegex -> PartiallyParsedRegex
parseLeftovers = map convertChars
  where convertChars :: MaybeParsed -> MaybeParsed
        convertChars (Unparsed (c, OtherChar)) = Parsed (Single c)
        -- Let brackets, negations, and repeats pass through unchanged
        convertChars (Unparsed (c, LBracket)) = Unparsed (c, LBracket)
        convertChars (Unparsed (c, RBracket)) = Unparsed (c, RBracket)
        convertChars (Unparsed (c, Carat)) = Unparsed (c, Carat)
        convertChars (Unparsed (c, Star)) = Unparsed (c, Star)
        convertChars (Unparsed (c, Plus)) = Unparsed (c, Plus)
        -- Let parsed tokens pass through unchanged
        convertChars (Parsed token) = Parsed token
        -- Expect not to see any other type of character (not strictly necessary)
        convertChars other = error ("parse error when running parseLeftovers: " ++ show other)

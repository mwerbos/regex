module Regex.Compile where

import Regex.Data
import Regex.Util
import Data.Graph.Inductive(empty,Gr(..),insNodes,insEdge,insEdges,mkGraph,Node)
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
    Unparsed (Char, TokenType) |
    Parsed Token |
    -- Should not contain recursive PartiallyParsedGroups.
    -- TODO: Enforce this somehow? By adding another wrapper?
    PartiallyParsedGroup [MaybeParsed] IsNegated
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
  | first_aut == emptyAutomaton = trace ("or'ing empty with " ++ show second_aut) $ second_aut
  | second_aut == emptyAutomaton = trace ("or'ing empty with " ++ show first_aut) $ first_aut
  | otherwise = 
  trace ("########## or'ing " ++ show first_aut ++ " with " ++ show second_aut) $
  Automaton {
    stateMap = 
        trace ("got combined graph: " ++ show overall_graph ++ ", now adding e-transitions") $
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
            trace ("adding epsilon transitions: " ++ show epsilon_edges) $
            trace ("final state for first automaton: " ++ show (finalState first_aut)) $
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
makeMiniAutomaton (Group tokens) = trace ("***********mini automatons for this group: " ++ show (map makeMiniAutomaton tokens)) $
    trace ("or on first: " ++ show (orAutomatons emptyAutomaton (head $ map makeMiniAutomaton tokens))) $
    trace ("first: " ++ show (head $ map makeMiniAutomaton tokens)) $
    foldl orAutomatons emptyAutomaton $ map makeMiniAutomaton tokens
makeMiniAutomaton (NoneOf tokens) = Automaton {
  stateMap = insEdge (0, 1, T $ NoneOf tokens) $ insNodes [(0,()), (1,())] $ empty,
  finalState = 1
}
makeMiniAutomaton (Or t1 t2) = orAutomatons (makeMiniAutomaton t1) (makeMiniAutomaton t2)
makeMiniAutomaton (Repeated token) = connect_end_to_beginning $ makeMiniAutomaton token
  where connect_end_to_beginning automaton =
            automaton { stateMap = insEdge (finalState automaton, 0, Epsilon) (stateMap automaton) }
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
parseGroups tokens = map (innerParseGroup . maybeNegate) (makeGroupPiles tokens)

-- For each PartiallyParsed Group, if it starts with a carat,
-- negates the group.
maybeNegate :: MaybeParsed -> MaybeParsed
maybeNegate (PartiallyParsedGroup ((Unparsed (_, Carat)):rest) Unnegated) =
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
        grab_chars (NoGroup,parsed) (Unparsed (_,LBracket)) = (IncompleteGroup [],parsed)
        grab_chars (_,parsed) (Unparsed (_,LBracket)) = error "Encountered [ inside group"
        grab_chars (NoGroup,_) (Unparsed (_,RBracket)) = error "Encountered ] outside group"
        grab_chars (IncompleteGroup group, parsed) (Unparsed (_,RBracket)) =
            (NoGroup, parsed ++ [PartiallyParsedGroup group Unnegated])
        grab_chars (IncompleteGroup group, parsed) next_token =
            (IncompleteGroup (group ++ [next_token]), parsed)
        grab_chars (NoGroup, parsed) next_token = (NoGroup, parsed ++ [next_token])

-- If it's a group, parses the characters inside the group.
-- Only wildcards and escapes are allowed.
innerParseGroup :: MaybeParsed -> MaybeParsed
innerParseGroup (PartiallyParsedGroup token_tuples Unnegated) =
    Parsed (Group 
        (forceParsed $ parseLeftovers $ parseWildcards $ parseEscapes token_tuples))
innerParseGroup (PartiallyParsedGroup token_tuples Negated) =
    Parsed (NoneOf
        (forceParsed $ parseLeftovers $ parseWildcards $ parseEscapes token_tuples))
innerParseGroup other = other


parseRepeats :: PartiallyParsedRegex -> PartiallyParsedRegex
parseRepeats (Unparsed (c,Star):Parsed token:rest) = (Parsed (Repeated token)):parseRepeats rest
parseRepeats (Unparsed (c,Plus):Parsed token:rest) = (Parsed (Repeated token)):Parsed token:parseRepeats rest
parseRepeats (Unparsed (c,Star):_:rest) = error "Unexpected * after unparsed input"
parseRepeats (Unparsed (c,Plus):_:rest) = error "Unexpected + after unparsed input"
parseRepeats (Unparsed (c,Star):[]) = error "Unexpected * at beginning of input"
parseRepeats (Unparsed (c,Plus):[]) = error "Unexpected + at beginning of input"
parseRepeats (other:rest) = other:parseRepeats rest
parseRepeats [] = []

parseWildcards :: PartiallyParsedRegex -> PartiallyParsedRegex
parseWildcards = map convertDots
    where convertDots :: MaybeParsed -> MaybeParsed
          convertDots (Unparsed ('.',Dot)) = Parsed Wildcard
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

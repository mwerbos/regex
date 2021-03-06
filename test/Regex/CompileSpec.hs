module Regex.CompileSpec where

import SpecHelper
import Regex.Compile
import Control.Exception (evaluate)
import Data.Graph.Inductive.Graph (mkGraph,Node)

nLengthNodes :: Int -> [(Node,())]
nLengthNodes n = take n $ zip [0..] (repeat ())

spec :: Spec
spec = do
  describe "processRegex" $ do
    it "processes a regex with a repeated token" $
      processRegex (Regex "a*") `shouldBe` Automaton {
        stateMap = mkGraph [(0,()), (1,())]
                           [(0,1,T $ Single 'a'), (1,0,Epsilon),
                            (0,1,Epsilon)],
        finalState = 1
      }
    it "processes a more complex regex with a repeated token" $
      processRegex (Regex "ba+c") `shouldBe` Automaton {
        stateMap = mkGraph (nLengthNodes 8)
                           [(0,1,T $ Single 'b'), (1,2,Epsilon),
                            (2,3,T $ Single 'a'), (3,4,Epsilon),
                            (4,5,T $ Single 'a'), (5,6,Epsilon),
                            (4,5,Epsilon),(5,4,Epsilon),
                            (6,7,T $ Single 'c')],
        finalState = 7
      }
    it "correctly processes a regex with a negated character class" $ do
      let regex = Regex "[^yd]o"
          simple_automaton = Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,())]
                               [(0,1,T (NoneOf [Single 'y', Single 'd'])),
                                (1,2,Epsilon),
                                (2,3,T $ Single 'o')],
            finalState = 3
          }
      processRegex regex `shouldBe` simple_automaton
    it "processes a regex with two character classes" $ do
      let regex = Regex "[yd]o[gb]"
          automaton = Automaton {
            stateMap = mkGraph (nLengthNodes 14)
                               [(0,2,Epsilon), (0,4,Epsilon),
                                (2,3,T $ Single 'y'), (4,5,T $ Single 'd'),
                                (3,1,Epsilon), (5,1,Epsilon),
                                (1,6,Epsilon), (6,7,T $ Single 'o'),
                                (7,8,Epsilon),
                                (8,10,Epsilon), (8,12,Epsilon),
                                (10,11,T $ Single 'g'), (12,13,T $ Single 'b'),
                                (11,9,Epsilon), (13,9,Epsilon)],
            finalState = 9
          }
      processRegex regex `shouldBe` automaton
    it "processes a regex with an escaped backslash" $
      processRegex (Regex "\\\\") `shouldBe` Automaton {
            stateMap = mkGraph [(0,()), (1,())] [(0,1,T $ Single '\\')],
            finalState = 1
          }
    it "processes a regex with an escaped bracket" $
      processRegex (Regex "\\[") `shouldBe` Automaton {
            stateMap = mkGraph [(0,()), (1,())] [(0,1,T $ Single '[')],
            finalState = 1
          }
    it "processes a regex with an escaped bracked *inside* a character class" $ do
      let automaton = Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,())]
                               [(0,2,Epsilon), (0,4,Epsilon),
                                (2,3,T $ Single ']'), (4,5,T $ Single '['),
                                (3,1,Epsilon), (5,1,Epsilon)],
            finalState = 1
          }
      processRegex (Regex "[\\]\\[]") `shouldBe` automaton
  describe "tokenize" $ do
    it "tokenizes a simple expression" $
      tokenize (Regex "hi") `shouldBe` [('h', OtherChar), ('i', OtherChar)]
    it "tokenizes with a repeated character" $
      tokenize (Regex "a*") `shouldBe` [('a', OtherChar), ('*', Star)]
    it "tokenizes an expression with negated character class" $ do
      let regex = Regex "[^yd]o"
      tokenize regex `shouldBe` [('[', LBracket), ('^', Carat),
                                 ('y', OtherChar), ('d', OtherChar),
                                 (']', RBracket), ('o', OtherChar)]
  describe "parse" $ do
    it "parses a simple expression" $ do
      let tokenized = [('f', OtherChar), ('o', OtherChar), ('x', OtherChar)]
      parse tokenized `shouldBe` [Single 'f', Single 'o', Single 'x']
    it "parses with a repeated token" $
      parse [('a', OtherChar), ('*', Star)] `shouldBe` [Repeated (Single 'a')]
    it "parses with a negated character class" $ do
      let tokenized = [('[', LBracket), ('^', Carat),
                       ('y', OtherChar), ('d', OtherChar),
                       (']', RBracket), ('o', OtherChar)]
      parse tokenized `shouldBe` [NoneOf [Single 'y', Single 'd'], Single 'o']
    it "parses a character group" $ do
      let tokenized = [('(', LParen), ('a', OtherChar), ('|', Pipe),
                       ('b', OtherChar), (')', RParen)]
      parse tokenized `shouldBe` [Or [Single 'a'] [[Single 'b']]]
    it "has correct breakdown in when it parses a group" $ do
      let tokenized = [('(', LParen), ('a', OtherChar), ('|', Pipe),
                       ('b', OtherChar), (')', RParen)]
          partially_parsed = [Unparsed ('(', LParen), Parsed (Single 'a'),
                              Unparsed ('|', Pipe), Parsed (Single 'b'),
                              Unparsed (')', RParen)]
      (parseCharacterClasses . parseLeftovers . parseWildcards . parseEscapes . makeMaybeParsed)
          tokenized `shouldBe` partially_parsed
      (forceParsed . parseRepeats . parseGroups) partially_parsed
          `shouldBe` [Or [Single 'a'] [[Single 'b']]]
  describe "parseGroups" $
    it "parses with a simple group" $ do
      let partially_parsed = [Unparsed ('(', LParen), Unparsed ('a', OtherChar),
                              Unparsed ('|', Pipe), Unparsed ('b', OtherChar),
                              Unparsed (')', RParen)]
      parseGroups partially_parsed `shouldBe` [Parsed (Or [Single 'a'] [[Single 'b']])]
  describe "makeGroupPiles" $
    it "makes piles with a simple group" $ do
      let partially_parsed = [Unparsed ('(', LParen), Unparsed ('a', OtherChar),
                              Unparsed ('|', Pipe), Unparsed ('b', OtherChar),
                              Unparsed (')', RParen)]
      makeGroupPiles partially_parsed `shouldBe`
          [PartiallyParsedGroup [Unparsed ('a', OtherChar)] [[Unparsed ('b', OtherChar)]]]
  describe "parseRepeats" $
    it "parses with a repeated token" $
      parseRepeats [Parsed (Single 'a'), Unparsed ('*', Star)] `shouldBe`
          [Parsed (Repeated (Single 'a'))]

  describe "makeMiniAutomaton" $
    it "makes a mini automaton with a character class" $ do
      let token = CharacterClass [Single 'y', Single 'd']
          simple_automaton = Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,())]
                               [(0,2,Epsilon), (0,4,Epsilon),
                                (2,3,T $ Single 'y'), (4,5,T $ Single 'd'),
                                (3,1,Epsilon), (5,1,Epsilon)],
            finalState = 1
          }
      makeMiniAutomaton token `shouldBe` simple_automaton
  describe "build automaton" $ do
    it "builds a simple automaton" $ do
      let parsed = [Single 'h', Single 'i']
          simple_automaton = Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,())]
                               [(0,1,T $ Single 'h'), (1,2,Epsilon), (2,3,T $ Single 'i')],
            finalState = 3
          }
      makeAutomaton parsed `shouldBe` simple_automaton
    it "builds an automaton with a character class" $ do
      let parsed = [CharacterClass [Single 'y', Single 'd'], Single 'o']
          simple_automaton = Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,()), (6,()), (7,())]
                               [(0,2,Epsilon), (0,4,Epsilon),
                                (2,3,T $ Single 'y'), (4,5,T $ Single 'd'),
                                (3,1,Epsilon), (5,1,Epsilon),
                                (1,6,Epsilon), (6,7,T $ Single 'o')],
            finalState = 7
          }
      makeAutomaton parsed `shouldBe` simple_automaton
    it "builds an automaton with a negated character class" $ do
      let parsed = [NoneOf [Single 'y', Single 'd'], Single 'o']
          simple_automaton = Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,())]
                               [(0,1,T $ NoneOf [Single 'y', Single 'd']),
                                (1,2,Epsilon),
                                (2,3,T $ Single 'o')],
            finalState = 3
          }
      makeAutomaton parsed `shouldBe` simple_automaton
    it "builds a longer automaton" $ do
      let parsed = [Single 'f', Single 'o', Single 'x']
          simple_automaton = Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,())]
                               [(0,1,T $ Single 'f'), (1,2,Epsilon), (2,3,T $ Single 'o'),
                                (3,4,Epsilon), (4,5,T $ Single 'x')],
            finalState = 5
          }
      makeAutomaton parsed `shouldBe` simple_automaton
  describe "orAutomaton" $ do
    it "combines two simple automatons" $ do
      let first = Automaton {
            stateMap = mkGraph [(0,()), (1,())] [(0,1,T $ Single 'h')],
            finalState = 1
          }
          second = Automaton {
            stateMap = mkGraph [(0,()), (1,())] [(0,1,T $ Single 'i')],
            finalState = 1
          }
      orAutomatons first second `shouldBe` Automaton {
            stateMap = mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,())]
                               [(0,2,Epsilon), (0,4,Epsilon),
                                (2,3,T $ Single 'h'), (4,5,T $ Single 'i'),
                                (3,1,Epsilon), (5,1,Epsilon)],
            finalState = 1
          }
    it "has an identity of 'emptyAutomaton'" $ do
      let aut = Automaton {
            stateMap = mkGraph [(0,()), (1,())] [(0,1,T $ Single 'h')],
            finalState = 1
          }
      orAutomatons aut emptyAutomaton `shouldBe` aut
      orAutomatons emptyAutomaton aut `shouldBe` aut
  describe "combineThreeGraphs" $ 
    it "combines three graphs correctly" $ do
      let first = mkGraph [(0,()), (1,())] [(0,1,T $ Single 'h')]
          second = mkGraph [(0,()), (1,())] [(0,1,T $ Single 'i')]
          third = mkGraph [(0,()), (1,())] [(0,1,T $ Single 'd')]
          (combined, (fn1, fn2, fn3)) = combineThreeGraphs (first, second, third)
      combined `shouldBe` mkGraph [(0,()), (1,()), (2,()), (3,()), (4,()), (5,())]
                                  [(0,1,T $ Single 'h'), (2,3,T $ Single 'i'), (4,5,T $ Single 'd')]
      fn1 0 `shouldBe` 0
      fn1 1 `shouldBe` 1
      fn2 0 `shouldBe` 2
      fn2 1 `shouldBe` 3
      fn3 0 `shouldBe` 4
      fn3 1 `shouldBe` 5

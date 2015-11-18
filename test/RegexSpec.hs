module RegexSpec where

import SpecHelper
import Regex
import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "matchExpression" $ do
    context "finding simple matches" $ do
      it "correctly finds a single match" $ do
        let text = "quick brown fox jumped over"
            regex = Regex "fox"
        
        matchExpression regex text `shouldBe` [Interval (12,15)]

      it "finds multiple matches in order" $ do
        let text = "quick brown fox jumped over fox yes"
            regex = Regex "fox"

        matchExpression regex text `shouldBe` [Interval (12,15), Interval (28,31)]

      it "finds overlapping matches in order" $ do
        let text = "ababa"
            regex = Regex "aba"

        matchExpression regex text `shouldBe` [Interval (0,3), Interval (2,5)]

      it "successfully matches an expression at the end of a string" $ do
        let text = "the fox"
            regex = Regex "fox"
        matchExpression regex text `shouldBe` [Interval (4,7)]

      it "finds no matches when there are none" $ do
        let text = "@@!4298sdaadww"
            regex = Regex "fox"
        matchExpression regex text `shouldBe` []

    context "matching with character classes" $ do
      it "matches using a character class" $ do
        let text1 = "France is a menace."
            text2 = "i hate france"
            regex = Regex "[Ff]rance"
        matchExpression regex text1 `shouldBe` [Interval (0,6)]
        matchExpression regex text2 `shouldBe` [Interval (7,13)]
    
      it "matches using multiple character classes" $ do
        let text1 = "France is a menace."
            text2 = "i hate francia"
            regex = Regex "[Ff]ranc[ei][ a]"
        matchExpression regex text1 `shouldBe` [Interval (0,7)]
        matchExpression regex text2 `shouldBe` [Interval (7,14)]
    
      it "matches weird things" $ do
        let text = "agog ogg"
            regex = Regex "[og]og"
        matchExpression regex text `shouldBe` [Interval (1,4)]

      it "matches escaped square brackets correctly" $ do
        let text = "I am [Duke of Aragon]."
            regex = Regex "[\\]\\[]"
        matchExpression regex text `shouldBe` [Interval (5,6), Interval (20,21)]

      it "matches escaped backslashes correctly" $ do
        let text = "We waged war, \\ and now our son is King of Castille."
             -- Two backslashes give Haskell one backslash each.
             -- So four backslashes here means one escaped backslash for my regex matcher.
            regex = Regex "\\\\"
        matchExpression regex text `shouldBe` [Interval (14,15)]
    
    context "catching user error for malformed matches" $ do
      it "fails on an unclosed character class" $ do
        let text = "I am [Duke of Aragon]."
            regex = Regex "[\\]\\["
        evaluate (matchExpression regex text) `shouldThrow`
            errorCall "Expected ] before end of regex"

      it "fails on an unfinished escaped character" $ do
        let text = "I am [Duke of Aragon.\\"
            regex = Regex "n.\\"
        evaluate (matchExpression regex text) `shouldThrow`
            errorCall "parse error when running parseLeftovers: Bare ('\\',Backslash)"

      it "fails on a wrongly escaped character" $ do
        let text = "I am [Duke of Aragon.\\"
            regex = Regex "\\a"
        evaluate (matchExpression regex text) `shouldThrow` 
            errorCall "Character 'a' may not be escaped"

      it "fails on surprising ] character" $ do
        let text = "I am ]Duke of Aragon.\\"
            regex = Regex "][\\]\\[]"
        evaluate (matchExpression regex text) `shouldThrow`
            errorCall "Encountered ] outside group"

      it "fails on surprising [ character" $ do
        let text = "I am Duke of Aragon.\\"
            regex = Regex "[[Duke"
        evaluate (matchExpression regex text) `shouldThrow`
            errorCall "Encountered [ inside group"

      it "treats unescaped carats not at the beginning of the character class as an error" $ do
        let text = "Duke of Aragon"
            regex = Regex "[abc^]"
        evaluate (matchExpression regex text) `shouldThrow`
            errorCall "unexpected ^ not at the beginning of a character class"

    context "matching with negated character classes" $ do
      it "does it" $ do
        let text = "dog bog log"
            regex = Regex "[^abcd]og"
        matchExpression regex text `shouldBe` [Interval (8,11)]

      it "allows escaped ^ outside character classes" $ do
        let text = "dog ^og bog"  
            regex = Regex "\\^o"
        matchExpression regex text `shouldBe` [Interval (4,6)]

      it "allows you to escape carats inside character classes" $ do
        let text = "dog ^og bog"
            regex = Regex "[ab\\^c]og"
        matchExpression regex text `shouldBe` [Interval (4,7), Interval (8,11)]

      it "matches a complex thing" $ do
        let text = "Roger wants to be the king of France."
            regex1 = Regex "o[^er][er][er]"
            regex2 = Regex "[b][^ ] t[the]e[ ke][kabc]"
        matchExpression regex1 text `shouldBe` [Interval (1,5)]
        matchExpression regex2 text `shouldBe` [Interval (15,23)]

    context "matching with a greedy + operator" $ do
      it "uses + operator" $ do
        let text = "i am dooog"
            regex = Regex "do+g"
        matchExpression regex text `shouldBe` [Interval (5,10)]

      it "disallows + operator in character class" $ do
        let text = "i am dogogogo"
            regex = Regex "d[og+]g"
        evaluate (matchExpression regex text) `shouldThrow`
            errorCall "can't use + character inside a character class"

      it "disallows two + in a row" $ do
        let text = "i am dooog"
            regex = Regex "do++g"
        evaluate (matchExpression regex text) `shouldThrow`
            errorCall "unexpected + after unparsed input"

      it "doesn't match 0 instances of repeated class" $ do
        let text = "i am dg"
            regex = Regex "do+g"
        matchExpression regex text `shouldBe` []

      it "prefers greediness over many matches" $ do
        let text = "ggggg"
            regex = Regex "g+"
        matchExpression regex text `shouldBe` [Interval (0,5)]

      it "works correctly when last token is greedy" $ do
        let text = "i am dog yes"
            regex = Regex "d[go]+"
        matchExpression regex text `shouldBe` [Interval (5,8)]

      it "uses + operator with a character class and is greedy" $ do
        let text = "i am dogogogo"
            regex = Regex "d[og]+g"
        matchExpression regex text `shouldBe` [Interval (5,12)]

      it "allows escaped +" $ do
        let text = "i am dog+gog yes"
            regex = Regex "d[og\\+]+g"
        matchExpression regex text `shouldBe` [Interval (5,12)]

      it "isn't greedy across things that don't match" $ do
        let text = "i am do+g yes dog"
            regex = Regex "d[o\\+]+g"
        matchExpression regex text `shouldBe` [Interval (5,9), Interval (14,17)]

      it "is greedy with negated character classes" $ do
        let text = "i am do+g yes dog"
            regex = Regex "d[^ab]+o+g"
        matchExpression regex text `shouldBe` [Interval (5,17)]

      it "handles lengths correctly with greediness, non-duplication" $ do
        let text = "abaabac"
            regex = Regex "a+bac"
        matchExpression regex text `shouldBe` [Interval (2, 7)]

      it "handles lengths correctly with greediness" $ do
        let text = "oogoooogggoogga"
            regex = Regex "o+g+o+g+a"
        matchExpression regex text `shouldBe` [Interval (3,15)]

    context "matching with a wildcard character" $ do
      it "matches anything" $ do
        let text = "hello"
            regex = Regex "h.llo"
        matchExpression regex text `shouldBe` [Interval (0,5)]

      it "escapes ." $ do
        let text = "h.llo"
            regex = Regex "h\\.llo"
        matchExpression regex text `shouldBe` [Interval (0,5)]

      it "greedy matches with a ." $ do
        let text = "hello"
            regex = Regex ".+o"
        matchExpression regex text `shouldBe` [Interval (0,5)]

    context "matching with char groups" $ do
      it "uses a character group 2 long" $ do
        let text = "abcbcd"
            regex = Regex "abc+d"
        matchExpression regex text `shouldBe` [Interval (0,6)]

main :: IO ()
main = hspec spec

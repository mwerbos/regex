module Regex.CompileSpec where

import SpecHelper
import Regex.Compile
import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "tokenize" $ do
    it "tokenizes a simple expression" $ do
      let regex = Regex "fox"
      tokenize regex `shouldBe` [('f', OtherChar), ('o', OtherChar), ('x', OtherChar)]

main :: IO ()
main = hspec spec

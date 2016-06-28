{-# LANGUAGE OverloadedStrings #-}

module Lisp.LispValSpec ( spec ) where

import           Data.Text
import           Lisp.LispVal
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  let foo = Symbol "foo"
  let bar = Symbol "bar"

  describe "Symbol" $ do
    it "generates equal symbols with equal arguments" $
      foo `shouldBe` Symbol "foo"

    it "QuickCheck generates equal symbols with equal arguments" $
       property $ \s -> let t = pack s in Symbol t == Symbol t

    it "generates different symbols with different arguments" $
      foo `shouldNotBe` bar

  describe "Int" $ do
    it "generates equal numbers with equal arguments" $
       Int 5 `shouldBe` Int 5

    it "QuickCheck generates equal ints with equal arguments" $
      property $ \s -> Int s == Int s

    it "generates different LispVals with different numbers" $
      Int 5 `shouldNotBe` Int 6

  describe "Pair" $
    it "Dotted pairs are equal if both the cdr and car are equal" $
      Pair foo bar `shouldBe` Pair foo bar

  describe "Helper Functions" $ do
    it "convert from Lisp List to Haskell List" $
      l2h (Pair foo (Pair bar Nil)) `shouldBe` [foo, bar]
    it "convert from a singleton List to Haskell List" $
      l2h (Pair foo Nil) `shouldBe` [foo]

    it "convert from Haskell List to Lisp List" $
      h2l [foo, bar] `shouldBe` (Pair foo (Pair bar Nil))
    it "converts a singleton list to a Lisp singleton list" $
      h2l [foo] `shouldBe` (Pair foo Nil)



{-# LANGUAGE OverloadedStrings #-}
module Lisp.EvalSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Text

import           Lisp

spec :: Spec

spec = do
  describe "Basic primitives self-eval" $ do
    it "Ints self-eval" $
      property $ \i -> let l = Int i in eval l == l

    it "Reals self-eval" $
      property $ \r -> let l = Real r in eval l == l

    it "Rationals self-eval" $
      property $ \q -> let l = Rational q in eval l == l

    it "Complex numbers self-eval" $
      property $ \c -> let l = Complex c in eval l == l

    it "Strings self-eval" $
      property $ \s -> let l = String (pack s) in eval l == l

    it "Bools self-eval" $
      property $ \b -> let l = Bool b in eval l == l

  describe "quoted forms return their contents" $ do
    let quoted v = Pair (Symbol "quote") (Pair v Nil)
    let foo = Symbol "foo"
    let bar = Symbol "bar"
    let foobar = Pair foo bar
    it "'foo evaluates to foo" $
      eval (quoted foo) `shouldBe` foo
    it "'(foo . bar) evaluates to (foo . bar)" $
      eval (quoted foobar) `shouldBe` foobar

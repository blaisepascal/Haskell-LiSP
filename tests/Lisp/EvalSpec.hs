{-# LANGUAGE OverloadedStrings #-}
module Lisp.EvalSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Complex
import           Data.Ratio
import           Data.Text

import           Lisp
import           Lisp.LispVal    (h2l)

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


  describe "basic primitives" $ do
    it "Can add two Ints" $
      eval (h2l [Symbol "+", Int 5, Int 5]) `shouldBe` Int 10
    it "Can add two Reals" $
      eval (h2l [Symbol "+", Real 5.0, Real 5.0]) `shouldBe` Real 10.0
    it "Can add Ints and Reals" $
      eval (h2l [Symbol "+", Int 5, Real 5.0]) `shouldBe` Real 10.0
    it "Can add Rationals and Complex" $
      eval (h2l [Symbol "+", Rational (5%2), Complex (2.4:+3.1)]) `shouldBe` Complex (4.9:+3.1)



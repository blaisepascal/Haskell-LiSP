{-# LANGUAGE OverloadedStrings #-}
module Lisp.ReadSpec (spec) where

import Data.Complex
import Data.Ratio
import Data.Text
import Test.Hspec
import Control.Exception (evaluate)

import Lisp

spec :: Spec

spec = do
  let foo = Symbol "foo"
  let bar = Symbol "bar"
  
  describe "parses symbols" $ do
    it "parses a '$' as a symbol" $ 
      readL "$" `shouldBe` Symbol "$"
 
    it "parses a 'f' as a symbol" $ 
      readL "f" `shouldBe` Symbol "f"
 
    it "fails to parse '@'" $ 
      evaluate (readL "@") `shouldThrow` anyErrorCall
 
    it "parses 'foo' as a symbol" $ 
      readL "foo" `shouldBe` foo
  
    it "parses 'f@' as a symbol" $ 
      readL "f@" `shouldBe` Symbol "f@"
 
    it "ignores leading spaces" $ 
      readL "  bar" `shouldBe` bar
    
    it "ignores trailing spaces" $ 
      readL "foo  " `shouldBe` foo
 
  describe "parses numbers" $ do
    it "parses '123' as a number" $ 
      readL "123" `shouldBe` Int 123
 
    it "parses '2.13' as a number" $ 
      readL "3.14" `shouldBe` Real 3.14

    it "parses '3/4' as a number" $
      readL "3/4" `shouldBe` Rational (3%4)

    it "Rationals have to have integer numerators" $
      evaluate (readL "3.1/4") `shouldThrow` anyErrorCall

    it "Rationals have to have integer denominators" $
      evaluate (readL "3/4.5") `shouldThrow` anyErrorCall

    it "parses '3.0+4.0i' as a complex number" $
      readL "3.0+4.0i" `shouldBe` Complex (3.0:+4.0)

  describe "parse strings" $ do
    let aString = "a string"
    let quote t = cons '"' (snoc (replace "\"" "\\\"" t) '"') 
       
    it "parses \"a string\" as a string LispVal" $ 
      readL (quote aString) `shouldBe` String aString
 
  describe "parse booleans" $ do
    it "parses #t as a True LispVal" $ 
      readL "#t" `shouldBe` Bool True
 
    it "parses #f as a False LispVal" $ 
      readL "#f" `shouldBe` Bool False
 
    it "fails to parse #true" $ 
      evaluate (readL "#true") `shouldThrow` anyErrorCall

  describe "parse lists and pairs" $ do
    it "parses () as Nil" $
      readL "()" `shouldBe` Nil

    it "parses (foo . bar) as a dotted pair" $
      readL "(foo . bar)" `shouldBe` Pair foo bar

    it "dotted pairs have to be end of list" $
      evaluate (readL "(foo . bar baz)") `shouldThrow` anyErrorCall

    it "singleton lists get read as pair with cdr nil" $
      readL "(foo)" `shouldBe` Pair foo Nil

    it "longer lists get to be chains of pairs" $
      readL "(foo bar)" `shouldBe` Pair foo (Pair bar Nil)

    it "Longer lists can end in dotted pairs" $
      readL "(foo bar . foo)" `shouldBe` Pair foo (Pair bar foo)

    it "parses 'foo as (quote foo)" $
      readL "'foo" `shouldBe` Pair (Symbol "quote") (Pair foo Nil)

--    it "parses `foo as (quasiquote foo)" $
--      readL "`foo" `shouldBe` Pair (Symbol "quasiquote") (Pair foo Nil)

--    it "parses `(foo ,bar) as (quasiquote (foo (unquote bar)))" $
--      readL "`(foo ,bar)" `shouldBe` Pair (Symbol "quasiquote") (Pair foo (Pair (Symbol "unquote") (Pair bar Nil)))

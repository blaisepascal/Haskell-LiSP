{-# LANGUAGE OverloadedStrings #-}


import Test.Hspec
import Test.QuickCheck
import Data.Text
import Control.Exception (evaluate)

import Lisp

main :: IO ()
main = hspec $ do
  let foo = Symbol "foo"
  let bar = Symbol "bar"
  describe "LispVal.Symbol" $ do
    
    it "generates equal symbols with equal arguments" $ 
      foo `shouldBe` Symbol "foo"

    it "QuickCheck generates equal symbols with equal arguments" $
       property $ \s -> let t = pack s in Symbol t == Symbol t

    it "generates different symbols with different arguments" $ 
      foo `shouldNotBe` bar

  describe "LispVal.Int" $ do
    it "generates equal numbers with equal arguments" $ 
       Int 5 `shouldBe` Int 5

    it "QuickCheck generates equal ints with equal arguments" $ 
      property $ \s -> Int s == Int s

    it "generates different LispVals with different numbers" $ 
      Int 5 `shouldNotBe` Int 6

  describe "LispVal.Pair" $ 
    it "Dotted pairs are equal if both the cdr and car are equal" $ 
      Pair foo bar `shouldBe` Pair foo bar

  describe "LispVal.pp" $ do
    it "prints symbols as its own value" $ 
      pp foo `shouldBe` "foo"

    it "QuickCheck symbols print as their own value" $ 
      property $ \s -> let t = pack s in pp (Symbol t) == t

    it "prints integers as their own value" $ 
      pp (Int 5) `shouldBe` "5"

    it "prints decimals as thier own value" $ 
      pp (Real 3.14) `shouldBe` "3.14"

    it "prints strings in quotes" $ 
      pp (String "foo") `shouldBe` "\"foo\""
    
    it "prints true as #t" $ 
      pp (Bool True) `shouldBe` "#t"

    it "prints false as #f" $ 
      pp (Bool False) `shouldBe` "#f"

    it "prints nils/empty lists as ()" $ 
      pp Nil `shouldBe` "()"

    it "prints pairs as dotted pairs" $ 
      pp (Pair foo bar) `shouldBe` "(foo . bar)"

    it "prints pairs with Nil cdr as singleton lists" $
      pp (Pair foo Nil) `shouldBe` "(foo)"
      
    it "prints lists as lists" $ 
      pp (Pair foo
           (Pair bar
            (Pair (Int 5)
             (Pair (Bool False) Nil)))) `shouldBe` "(foo bar 5 #f)"
      
    it "prints dotted lists with dots" $ 
      pp (Pair foo
           (Pair bar
            (Pair (Int 5)
             (Pair (Bool False) (Bool True))))) `shouldBe` "(foo bar 5 #f . #t)"
 
  describe "LispVal.readL" $ do
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
        
      

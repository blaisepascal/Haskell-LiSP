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
    
    it "generates equal symbols with equal arguments" $ do
      foo `shouldBe` (Symbol "foo")

    it "QuickCheck generates equal symbols with equal arguments" $
       property $ \s -> let t = pack s in (Symbol t) == (Symbol t)

    it "generates different symbols with different arguments" $ do
      foo `shouldNotBe` bar

  describe "LispVal.Int" $ do
    it "generates equal numbers with equal arguments" $ do
       (Int 5) `shouldBe` (Int 5)

    it "QuickCheck generates equal ints with equal arguments" $ do
      property $ \s -> (Int s) == (Int s)

    it "generates different LispVals with different numbers" $ do
      (Int 5) `shouldNotBe` (Int 6)

  describe "LispVal.pp" $ do
    it "prints symbols as its own value" $ do
      (pp foo) `shouldBe` "foo"

    it "QuickCheck symbols print as their own value" $ do
      property $ \s -> let t = pack s in (pp (Symbol t)) == t

    it "prints integers as their own value" $ do
      (pp (Int 5)) `shouldBe` "5"

    it "prints decimals as thier own value" $ do
      (pp (Real 3.14)) `shouldBe` "3.14"
    

  describe "LispVal.readL" $ do
    describe "parses symbols" $ do
      it "parses a '$' as a symbol" $ do
        (readL "$") `shouldBe` (Symbol "$")

      it "parses a 'f' as a symbol" $ do
        (readL "f") `shouldBe` (Symbol "f")

      it "fails to parse '@'" $ do
        evaluate (readL "@") `shouldThrow` anyErrorCall

      it "parses 'foo' as a symbol" $ do
        (readL "foo") `shouldBe` foo

      it "parses 'f@' as a symbol" $ do
        (readL "f@") `shouldBe` (Symbol "f@")

      it "ignores leading spaces" $ do
        (readL "  bar") `shouldBe` bar

      it "ignores trailing spaces" $ do
        (readL "foo  ") `shouldBe` foo

    describe "parses numbers" $ do
      it "parses '123' as a number" $ do
        (readL "123") `shouldBe` (Int 123)

      it "parses '2.13' as a number" $ do
        (readL "3.14") `shouldBe` (Real 3.14)

    describe "parse strings" $ do
      let aString = "a string"
      let quote t = cons '"' (snoc (replace "\"" "\\\"" t) '"') 
      
      it "parses \"a string\" as a string LispVal" $ do
        (readL $ quote aString) `shouldBe` (String aString)

    describe "parse booleans" $ do
      it "parses #t as a True LispVal" $ do
        (readL "#t") `shouldBe` (Bool True)

      it "parses #f as a False LispVal" $ do
        (readL "#f") `shouldBe` (Bool False)

      it "fails to parse #true" $ do
        evaluate (readL "#true") `shouldThrow` anyErrorCall
      

        

{-# LANGUAGE OverloadedStrings #-}


import Test.Hspec
import Test.QuickCheck
import Data.Text
import Control.Exception (evaluate)

import LispVal

main :: IO ()
main = hspec $ do
  describe "LispVal.Symbol" $ do
    let foo = Symbol "foo"
    let bar = Symbol "bar"
    
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
      (pp (Symbol "foo")) `shouldBe` "foo"

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
        (readL "foo") `shouldBe` (Symbol "foo")

      it "parses 'f@' as a symbol" $ do
        (readL "f@") `shouldBe` (Symbol "f@")

    describe "parses numbers" $ do
      it "parses '123' as a number" $ do
        (readL "123") `shouldBe` (Int 123)

      it "parses '2.13' as a number" $ do
        (readL "3.14") `shouldBe` (Real 3.14)
        

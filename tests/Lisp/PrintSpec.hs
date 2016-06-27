{-# LANGUAGE OverloadedStrings #-}
module Lisp.PrintSpec (spec) where

import Data.Complex
import Data.Ratio
import Data.Text
import Test.Hspec
import Test.QuickCheck

import Lisp

spec :: Spec

spec = do
  let foo = Symbol "foo"
  let bar = Symbol "bar"
  
  describe "LispVal.pp" $ do
    it "prints symbols as its own value" $ 
      pp foo `shouldBe` "foo"

    it "QuickCheck symbols print as their own value" $ 
      property $ \s -> let t = pack s in pp (Symbol t) == t

    it "prints integers as their own value" $ 
      pp (Int 5) `shouldBe` "5"

    it "prints decimals as thier own value" $ 
      pp (Real 3.14) `shouldBe` "3.14"

    it "prints rationals as their own value, lisp-style" $
      pp (Rational (3%4)) `shouldBe` "3/4"

    it "prints rationals reduced" $
      pp (Rational (4%8)) `shouldBe` "1/2"

    it "prints complex number as their own value, lisp-style" $
      pp (Complex (3:+4)) `shouldBe` "3.0+4.0i"

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
 

{-# LANGUAGE OverloadedStrings #-}
module Lisp.EvalSpec (spec) where

import           Data.Complex
import           Data.Ratio
import           Data.Text
import           Test.Hspec
import           Test.QuickCheck

import           Lisp
import           Lisp.Eval48

spec :: Spec

spec = do
  let foo = Symbol "foo"
  let bar = Symbol "bar"

  describe "Basic primitives self-eval" $ do
    it "Ints self-eval" $
      eval (Int 5) `shouldBe` Int 5


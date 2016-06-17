> {-# LANGUAGE OverloadedStrings #-}
> module Main ( main ) where

> import Test.HUnit
> import qualified System.Exit as Exit
> import Control.Monad
> import LispVal
>

> myTests :: Test
> myTests = TestList [

Let's make sure that LispVal equality works the way we expect.

>   TestLabel "Identical symbols are equal" (
>       TestCase $ assertBool "Symbol foo equals Symbol foo" (Symbol "foo" == Symbol "foo")
>       ),
>   TestLabel "Different symbols are different" (
>       TestCase $ assertBool "Symbol foo does not equals Symbol foo" (Symbol "foo" /= Symbol "bar")
>       ) 
>   ]
>   

> main :: IO ()
> main = do
>   count <- runTestTT myTests
>   when (failures count > 0) Exit.exitFailure
>   return ()

> {-# LANGUAGE OverloadedStrings #-}

This is the main module that defines what a Lisp value is

> module Lisp.LispVal (
>   LispVal(..), (<:>)
>   ) where
>
> import Data.Ratio
> import Data.Text
> import Data.Complex

The LispVal datatype itself. Lisp values can be symbols, strings, chars, numbers, booleans, nil
or cons pairs of Lisp values. 

> data LispVal =
>   Symbol Text
>   | Int Integer
>   | Real Double
>   | Rational Rational
>   | Complex (Complex Double)
>   | String Text
>   | Bool Bool
>   | Nil -- empty list
>   | Pair LispVal LispVal
>   deriving (Show, Eq)


Unfortunately, to avoid name collisions with other Haskell modules, I need
to rename these functions.

Some helper constructor functions:

> infixl 5 <:>
> (<:>)  :: LispVal -> LispVal -> LispVal
> (<:>) = Pair

> toLisp :: [LispVal] -> LispVal
> toLisp [] = Nil

toLisp [x:xs] = Pair x (toLisp xs)


> {-# LANGUAGE OverloadedStrings #-}

This is the main module that defines what a Lisp value is

> module Lisp.LispVal (
>   LispVal(..)
>   ) where
>
> import Data.Text

The LispVal datatype itself. Lisp values can be symbols, strings, chars, numbers, booleans, nil
or cons pairs of Lisp values. 

> data LispVal =
>   Symbol Text
>   | Int Integer
>   | Real Double
>   | String Text
>   | Bool Bool
>   deriving (Show, Eq)


Unfortunately, to avoid name collisions with other Haskell modules, I need
to rename these functions.


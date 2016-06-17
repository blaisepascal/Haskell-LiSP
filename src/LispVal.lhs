> {-# LANGUAGE OverloadedStrings #-}

This is the main module that defines what a Lisp value is, how to print it, and how to read it.
This module does not handle evaluation

> module LispVal (
>   LispVal(Symbol),
>   pp
>   ) where
>
> import Data.Text
> -- import Data.Attoparsec.Text

The LispVal datatype itself. Lisp values can be symbols, strings, chars, numbers, booleans, nil
or cons pairs of Lisp values. 

> data LispVal =
>   Symbol Text
>   deriving (Show, Eq)

The main Lisp loop is the "read eval print" loop. As such,
we should have functions to do those items. Ideally, the
signatures of those functions should be:


read :: Text -> LispVal
eval :: LispVal -> LispVal
print :: LispVal -> Text

For now, read will return an Attoparsed Result LispVal to
capture failures.

Print is called "pp". Ideally, read (print l) == l. This means
that symbols should print as bare words, strings as quoted
strings, booleans as #t and #f, etc.

> pp :: LispVal -> Text
> pp l = case l of
>   Symbol t -> t

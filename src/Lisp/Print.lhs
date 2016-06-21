> {-# LANGUAGE OverloadedStrings #-}
> module Lisp.Print (pp) where

> import Lisp.LispVal
> import Data.Text
> import Text.Printf
> import Data.Ratio
> import Data.Complex

Print is called "pp". Ideally, read (print l) == l. This means
that symbols should print as bare words, strings as quoted
strings, booleans as #t and #f, etc.

> pp :: LispVal -> Text
> pp l = case l of
>   Symbol t -> t
>   Int n -> pack $ show n
>   Rational r -> pack $ printf "%v/%v" (numerator r) (denominator r)
>   Real r -> pack $ show r
>   Complex c -> pack $ printf "%v+%vi" (realPart c) (imagPart c)
>   String s -> quote s
>   Bool b -> if b then "#t" else "#f"
>   Nil -> "()"
>   p@(Pair _ _) -> "(" `append` (printBareList p) `append` ")"

Lists need careful work. They are made of chained pairs, where each cdr except the last is another pair, as in
(Pair (Symbol "foo") (Pair (Symbol "bar") (Pair (Symbol "Baz") end))). If end is Nil, it's a "list", and would be printed
as (foo bar baz). If end is not a Nil, it's a "dotted list", and would get printed as (foo bar baz . end).

> printBareList :: LispVal -> Text
> printBareList l = case l of
>   Pair a Nil          -> pp a
>   Pair a p@(Pair _ _) -> (pp a) `append` " " `append` (printBareList p)
>   Pair a b            -> (pp a) `append` " . " `append` (pp b)

> quote :: Text -> Text
> quote t = '"' `cons` t `snoc` '"' 


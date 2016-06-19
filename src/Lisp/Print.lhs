> module Lisp.Print (pp) where

> import Lisp.LispVal
> import Data.Text

Print is called "pp". Ideally, read (print l) == l. This means
that symbols should print as bare words, strings as quoted
strings, booleans as #t and #f, etc.

> pp :: LispVal -> Text
> pp l = case l of
>   Symbol t -> t
>   Int n -> pack $ show n
>   Real r -> pack $ show r


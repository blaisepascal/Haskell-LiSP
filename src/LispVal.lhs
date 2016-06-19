> {-# LANGUAGE OverloadedStrings #-}

This is the main module that defines what a Lisp value is, how to print it, and how to read it.
This module does not handle evaluation

> module LispVal (
>   LispVal(Symbol, Int, Real),
>   pp, readL
>   ) where
>
> import Data.Text
> import Data.Attoparsec.Text
> import Control.Applicative

The Lisp code is divided into separate m

The LispVal datatype itself. Lisp values can be symbols, strings, chars, numbers, booleans, nil
or cons pairs of Lisp values. 

> data LispVal =
>   Symbol Text
>   | Int Integer
>   | Real Double
>   deriving (Show, Eq)

The main Lisp loop is the "read eval print" loop. As such,
we should have functions to do those items. Ideally, the
signatures of those functions should be:


read :: Text -> LispVal
eval :: LispVal -> LispVal
print :: LispVal -> Text

Unfortunately, to avoid name collisions with other Haskell modules, I need
to rename these functions.

Print is called "pp". Ideally, read (print l) == l. This means
that symbols should print as bare words, strings as quoted
strings, booleans as #t and #f, etc.

> pp :: LispVal -> Text
> pp l = case l of
>   Symbol t -> t
>   Int n -> pack $ show n
>   Real r -> pack $ show r

Read is called "readL", to avoid conflicting with existing read functions. It is not a parser, but it calls
parsers.
   
> readL :: Text -> LispVal
> readL input = case parseOnly parseLispVal input of
>   Left err -> error err
>   Right a -> a

A LispVal can be a symbol, a number, a bool, nil, or a pair/list.

> parseLispVal :: Parser LispVal
> parseLispVal = parseSymbol <|> parseNumber

Parsing a Scheme symbol can be complicated. According to R6RS, "[i]n general, a sequence of letters,
digits, and 'extended alphabetic characters' is an identifier when it begins with a character that cannot being
a representation of a number object. In addition, +, -, and ... are identifiers, as is a sequence of letters, digits,
and extended alphabetic characters that begins wth the two-character sequence ->". Formally, it defines an identifier
as composed of an "initial" followed by many "subsequents", or as a "peculiar identifier", where an "initial" is a
"constituent", "special initial", or an "inline hex escape", and so on down the rabbit hole. "Subsequent"s adds digits
and a few other added characters.

> initial :: Parser Char
> initial = letter <|> specialinitial

> specialinitial :: Parser Char
> specialinitial = satisfy $ inClass "!$%&*+/:<=>?^_~"
>
> subsequent :: Parser Char
> subsequent = initial <|> digit <|> specialsubsequent
>
> specialsubsequent :: Parser Char
> specialsubsequent = satisfy $ inClass "+.@-"
>
> identifier :: Parser Text
> identifier = do
>   i <- initial
>   rest <- many' subsequent
>   return $ pack $ i:rest
>
> parseSymbol :: Parser LispVal
> parseSymbol = do
>   id <- identifier
>   return $ Symbol id

A lisp number can be an integer, a float, a fraction. Unfortunately, these are distinct types in Haskell, so
they have to be treated separately.

> parseNumber :: Parser LispVal
> parseNumber = do
>   num <- number  -- Note: 'number' parser is deprecated in favor of 'scientific'
>   case num of
>     I i -> return $ Int i
>     D d -> return $ Real d

   



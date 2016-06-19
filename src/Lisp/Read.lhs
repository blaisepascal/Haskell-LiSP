> {-# LANGUAGE OverloadedStrings #-}
> module Lisp.Read (readL) where

> import Data.Text
> import Data.Attoparsec.Text
> import Control.Applicative
> import Lisp.LispVal
> import Data.Scientific (floatingOrInteger)

Read is called "readL", to avoid conflicting with existing read functions. It is not a parser, but it calls
parsers.
   
> readL :: Text -> LispVal
> readL input = case parseOnly parseLispVal input of
>   Left err -> error err
>   Right a -> a

A LispVal can be a symbol, a number, a bool, nil, or a pair/list.

> parseLispVal :: Parser LispVal
> parseLispVal = do
>   skipSpace
>   parseSymbol
>   <|> parseNumber
>   <|> parseString
>   <|> parseBool

Many LispVals can be followed by a delimiter, like (if #t(true case) (false case)), and don't
require whitespace or other gap. So let's add a "delimiter" parser for lookahead purposes


> delimiter :: Parser Char
> delimiter = do
>   space <|> (satisfy $ inClass "()[]\";#") <|> (endOfInput >> return 'x')


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
>   num <- scientific  -- Note: 'number' parser is deprecated in favor of 'scientific'
>   case (floatingOrInteger) num of
>     Right i -> return $ Int i
>     Left  d -> return $ Real d

A lisp string is quoted, with certain escape sequenced indicated by a \

> parseString :: Parser LispVal
> parseString = do
>   char '"'
>   s <- many (notChar '"')
>   char '"'
>   return $ String $ pack s

A lisp boolean is either #t or #f for True and False, respectively

> parseBool :: Parser LispVal
> parseBool = do
>   b <- eitherP (string "#t") (string "#f")
>   delimiter
>   return $ case b of
>     Left _ -> Bool True
>     _ -> Bool False
>                               

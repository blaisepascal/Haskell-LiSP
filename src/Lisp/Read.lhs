> {-# LANGUAGE OverloadedStrings #-}
> module Lisp.Read (readL) where

> import Data.Text
> import Data.Ratio
> import Data.Complex
> import Data.Attoparsec.Text
> import Data.Attoparsec.Combinator
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
>   <|> parseNil
>   <|> parseList
>   <|> parseQuoted

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
> parseNumber = parseInteger <|> parseRational <|> parseReal <|> parseComplex
>
> parseInteger = do
>   num <- decimal
>   lookAhead delimiter
>   return $ Int num
>
> parseRational = do
>   num <- decimal
>   char '/'
>   den <- decimal
>   lookAhead delimiter
>   return $ Rational (num%den)
>
> parseReal = do
>   num <- double
>   lookAhead delimiter
>   return $ Real num
>
> parseComplex = do
>   r <- double
>   char '+'
>   i <- double
>   char 'i'
>   lookAhead delimiter
>   return $ Complex (r:+i)

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
>   lookAhead delimiter
>   return $ case b of
>     Left _ -> Bool True
>     _ -> Bool False
>                               

A lisp Nil is easy: It's (), and only ()

> parseNil :: Parser LispVal
> parseNil = do
>   string "()"
>   return Nil

Lisp lists are a bit more complicated. A dotted pair is of the form (foo . bar), but things get treated
specially if bar is another pair or nil. If it's a pair, then it's treated as a list, and more elements get,
a la (foo . (bar . baz)) => (foo bar . baz). If the last pair has a cdr of nil, then the dot and nil is
left out, (foo . ()) => (foo), (foo . (bar . (baz . ()))) => (foo bar baz).

When parsing, we need to do that backwards. (foo bar baz) => (foo . (bar . (baz . ()))), etc.

> parseList :: Parser LispVal
> parseList = do
>   (string "(") <?> "List open paren"
>   cdr <- parseBareList
>   skipSpace
>   (string ")") <?> "List close paren"
>   return cdr

> parseBareList :: Parser LispVal
> parseBareList =  parsePair <|> parseLongList <|> parseListEnd
>
> parsePair :: Parser LispVal
> parsePair = do
>   car <- parseLispVal
>   skipSpace
>   cdr <- "." *> parseLispVal
>   return $ Pair car cdr

> parseListEnd :: Parser LispVal
> parseListEnd = do
>   car <- parseLispVal
>   return $ Pair car Nil
>
> parseLongList :: Parser LispVal
> parseLongList = do
>   car <- parseLispVal
>   cdr <- parseBareList
>   return $ Pair car cdr

Quoted expressions are interesting: The expression 'foo is stored as (quote foo). It was
uncertain to me if there could be a space (e.g., is it OK to do '(foo ' bar) to get (quote (foo (quote bar)))
or is that an error. Gnu-Scheme seems to think it's OK.

> parseQuoted :: Parser LispVal
> parseQuoted = do
>   char '\''
>   l <- parseLispVal
>   return $ Pair (Symbol "quote") (Pair l Nil)
>   

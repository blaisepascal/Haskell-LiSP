> {-# LANGUAGE OverloadedStrings #-}

This is the main module that defines what a Lisp value is

> module Lisp.LispVal (
>   LispVal(..), l2h, h2l
>   ) where
>
> import Data.Ratio
> import Data.Text
> import Data.Complex

The LispVal datatype itself. Lisp values can be symbols, strings, chars, numbers, booleans, nil
or cons pairs of Lisp values.

Number types will crop up a lot, let's make some synonyms

> type Z = Integer
> type R = Double
> type Q = Rational
> type C = Complex Double

> data LispVal =
>   Symbol Text
>   | Int Z
>   | Real R
>   | Rational Q
>   | Complex C
>   | String Text
>   | Bool Bool
>   | Nil -- empty list
>   | Pair LispVal LispVal
>   deriving (Show, Eq)


Some helper constructor functions:

l2h will convert from a Lisp list to a Haskell list.

> l2h :: LispVal -> [LispVal]
> l2h Nil = []
> l2h (Pair a l) = a : (l2h l)
> 

h2l will do the reverse

> h2l :: [LispVal] -> LispVal
> h2l [] = Nil
> h2l (x:xs) = Pair x (h2l xs)

I also want to make it easy to do numeric operations on LispVals, as half the types of LispVals are types of
numbers. So let's make it a num. Unfortunately, this is not necessarily easy. I have to define what to do with each type,
and for binary operators, each type combination.

> instance Num LispVal where
>   (+) = lispifyOp (+) (+) (+) (+)
>   (-) = lispifyOp (-) (-) (-) (-)
>   (*) = lispifyOp (*) (*) (*) (*) 
>   abs = lispifyFun abs abs abs abs 
>   negate = lispifyFun negate negate negate negate
>   signum = lispifyFun signum signum signum signum

> type NumFun a = a -> a

> lispifyFun :: NumFun Z -> NumFun Q -> NumFun R -> NumFun C -> LispVal -> LispVal
> lispifyFun z _ _ _ (Int a) = Int (z a)
> lispifyFun _ q _ _ (Rational a) = Rational (q a)
> lispifyFun _ _ r _ (Real a) = Real (r a)
> lispifyFun _ _ _ c (Complex a) = Complex (c a)

> type BinNumOp a = a -> a -> a

> lispifyOp :: BinNumOp Z -> BinNumOp Q -> BinNumOp R -> BinNumOp C -> LispVal -> LispVal -> LispVal
> lispifyOp z _ _ _ (Int a)      (Int b)      = Int      $ a `z` b
> lispifyOp _ q _ _ (Rational a) (Rational b) = Rational $ a `q` b
> lispifyOp _ _ r _ (Real a)     (Real b)     = Real     $ a `r` b
> lispifyOp _ _ _ c (Complex a)  (Complex b)  = Complex  $ a `c` b
> lispifyOp z q r c (Int a)      b = lispifyOp z q r c (Rational $ fromInteger a) b
> lispifyOp z q r c (Rational a) b = lispifyOp z q r c (Real $ fromRational a) b
> lispifyOp z q r c (Real a)     b = lispifyOp z q r c (Complex (a:+0)) b

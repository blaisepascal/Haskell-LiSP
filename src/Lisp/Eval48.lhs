> {-# LANGUAGE OverloadedStrings #-}

> module Lisp.Eval48 (eval) where

Lisp Evaluator based on "Write Yourself A Lisp In 48 Hours"

> import Lisp.LispVal

The heart of any Lisp is eval and apply, which (respectively) evaluates Lisp expressions, and applies Lisp functions
to Lisp values. Eval is external, and apply may or may not be, I'm not sure.

> eval :: LispVal -> LispVal

Most primitives are self-evaluating: the value of 5 is 5, the value of "foo" is "foo", etc.

> quote = Symbol "quote"

> eval val@(Int _) = val
> eval val@(String _) = val
> eval val@(Real _) = val
> eval val@(Rational _) = val
> eval val@(Complex _) = val
> eval val@(Bool _) = val
> eval (Pair (Symbol "quote") (Pair val Nil)) = val

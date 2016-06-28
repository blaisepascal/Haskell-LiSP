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
> eval (Pair func args) = evalPair func (l2h args)

Pairs get evaluated in cases. Some keywords (special symbols) get special treatment, while otherwise it's basically
"look up the function, and apply it"

> evalPair :: LispVal -> [LispVal] -> LispVal
> evalPair (Symbol "quote") = head
> evalPair f = (apply f).(map eval)

apply takes a Lisp function and applies it to a list of args.

> apply :: LispVal -> [LispVal] -> LispVal
> apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives are a list of "primitive" operations.

> primitives :: [(LispVal, [LispVal] -> LispVal)]
> primitives = [(Symbol "+", foldl (+) (Int 0))]





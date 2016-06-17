QuickCheck tests for LispVals

> {-# LANGUAGE OverloadedStrings #-}
> module Main ( main ) where
> 
> import Test.QuickCheck
> import LispVal
> import Data.Text

Pretty-printing a symbol should yield the string used to make the symbol

> prop_pp_symbol_yields_contents :: String -> Bool
> prop_pp_symbol_yields_contents s =
>   let t = pack s in 
>     t == pp (Symbol t)

> main :: IO ()
> main =
>   quickCheck prop_pp_symbol_yields_contents


{-# LANGUAGE OverloadedStrings #-}

module Lisp (LispVal(..), readL, eval, pp) where

import           Lisp.Eval    (eval)
import           Lisp.LispVal
import           Lisp.Print
import           Lisp.Read


-- The main Lisp loop is the "read eval print" loop. As such,
-- we should have functions to do those items. Ideally, the
-- signatures of those functions should be:


-- read :: Text -> LispVal
-- eval :: LispVal -> LispVal
-- print :: LispVal -> Text

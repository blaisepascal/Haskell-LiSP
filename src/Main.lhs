A basic simple REPL for Scheme. Well, not quite a REPL yet. A REP, taking the expression from the command line.

> import Lisp
> import System.IO
> import System.Environment
> import Data.Text (pack)
>
> main :: IO ()
> main = getArgs >>= print . pp . eval . readL . pack . head
> 

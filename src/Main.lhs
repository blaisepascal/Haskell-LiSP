Initially we just want to test building and working
with literate Haskell. To begin with, we just want to
write a small "Hello World" program.

> import System.Environment
>
> main :: IO ()
> main =
>   putStrLn "Hello, World!"

If I understand this correctly, this should build and
execute correctly.

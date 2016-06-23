# Haskell-LiSP
LISP in Small Pieces, done in Haskell, eventually

This is a work-in-progress to follow the exercises in the book [*LISP in Small Pieces*](https://pages.lip6.fr/Christian.Queinnec/WWW/LiSP.html) (LiSP), but using Haskell as an implementation language rather than Scheme. This gives me an opportunity to learn both Haskell, and to learn about the inner workings of implementing Lisp/Scheme.

Because LiSP is using Scheme as an implementation language, it has some inherent advantages and disadvantages over a Haskell implementation. For advantages, Scheme comes with a built-in Lisp parser "read", meaning LiSP doesn't have to worry about parsing strings into lisp object (it also has lisp objects, and "print"); Scheme also has LiSP also uses set!, which updates state in ways not immediately obvious how to do in Haskell. For disadvantages, since the heart of LiSP is implementing (many different) versions of eval and apply, built-in functions in Scheme, it can't use those names.

To overcome the disadvantages, I am starting with the tutorial [Write Yourself A Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours), modifying it to meet my pedagogical needs as I go. It starts by writing a parser, then handling eval, maintaining environments, etc. Once I finish that, I should be effectively at the state of end of the first chapter of LiSP. Or so.

I am also using HSpec and QuickCheck for testing, something not really covered either in the tutorial or in LiSP. I tried to do LiSP earlier with tests, but it isn't clear from the writing (or the translation?) what new functionality was being added, making writing tests for that functionality hard to do. What, for instance, is a minimal test case to compare dynamic versus lexical binding? It's not clear from the text.

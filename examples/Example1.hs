{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-

I recommend doing this example using "ghci Example1.hs -ddump-splices".
Here's the output:

Example1.hs:1:1: Splicing declarations
    "\
    \main = print $ map (+(1 :: Int)) [1 :: Int ..10]"
  ======>
    [main
       = print
         $ app
             (app map (app (app Prelude.flip (+)) (1 :: Int))) [1 :: Int .. 10]]
Ok, modules loaded: Example1.
λ> main
[Int]
Int -> Int
[Int
Int -> Int -> Int
2,3,4,5,6,7,8,9,10,11]
-}
module Example1 where

import Language.Haskell.TH.OverloadApp

import Data.Typeable ( typeOf )
import Debug.Trace   ( traceShow )

app l r = traceShow (typeOf r) (l r)

[overloadApp|
main = print $ map (+(1 :: Int)) [1 :: Int ..10]
|]

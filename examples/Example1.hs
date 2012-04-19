{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Example1 where

import Language.Haskell.Meta.TransformEverywhere

import Data.Typeable ( typeOf )
import Debug.Trace   ( traceShow )

app l r = traceShow (typeOf r) (l r)

[overloadApp|
main = print $ map (+(1 :: Int)) [1 :: Int ..10]
|]
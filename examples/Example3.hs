{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns, ScopedTypeVariables #-}

import Language.Haskell.TH.StringSplice
import System.IO (stdout, hFlush)

prompt str = do
  putStr $ str ++ "? "
  hFlush stdout
  getLine

main = do
  name <- prompt "Name"
  putStrLn [s|Hello, {{name}}!|]
  putStrLn [s|3 + 3 = {{ show $ 3 + 3 }}|]
  putStrLn [s|5 ** 3 = {{ show $ 5 ** 3 }}|]

  [s|({{(read -> x :: Int)}},{{(read -> y :: Int)}})|] <- prompt "Give me coordinate!"
  putStrLn [s|Heading towards {{show x}} horizontal, {{show y}} vertical.|]
{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}

-- This implements a static interpreter for numeric literals

import Data.Data                    ( Data )
import Data.Generics.Aliases        ( extT )
import Data.Generics.Schemes        ( everywhere )
import Language.Haskell.TH
import Language.Haskell.TH.Builders
import Language.Haskell.TH.Fixity

evaluate :: Data a => a -> Q a
evaluate x = return . everywhere (id `extT` (eval . debug)) =<< resolveFixities x

eval :: Exp -> Exp
eval [e'| {{LitE (IntegerL a)}} + {{LitE (IntegerL b)}} |]
  = LitE . IntegerL $ a + b

eval [e'| {{LitE (IntegerL a)}} - {{LitE (IntegerL b)}} |]
  = LitE . IntegerL $ a - b

eval [e'| {{LitE (IntegerL a)}} * {{LitE (IntegerL b)}} |]
  = LitE . IntegerL $ a * b

eval [e'| negate {{LitE (IntegerL a)}} |]
  = LitE . IntegerL . negate $ a

eval [e'| ({{expr}}) |]
  = expr

eval e = e

evalQ = equasi (either error return . parseExp "")
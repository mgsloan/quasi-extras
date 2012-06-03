{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}

-- This implements a static interpreter for numeric literals

{- Example:

λ> [evalQ| if True then 1 + 4 else 9 * (30 - 2) + 100 |]
<interactive>:5:1-53: Splicing expression
    " if True then 1 + 4 else 9 * (30 - 2) + 100 "
  ======>
    if True then 5 else 352
5

-}

import Control.Applicative              ( (<$>), (<*>) )
import Data.Generics                    ( Data, everywhereM, extM )

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote        ( QuasiQuoter(..) )
import Language.Haskell.TH.Substitution ( Free, Subst )
import Language.Quasi.Ast.TH            ( e' )
import Language.Quasi.Internal.Desugar  ( dsParens, dsInfix )
import Language.Quasi.Internal.Fixity   ( resolveFixities )
import Language.Quasi.Internal.Utils    ( equasi', parseExp, qualifyNames, debug )

evalQ :: QuasiQuoter
evalQ = equasi' evaluate

evaluate :: (Subst a, Free a, Data a) => a -> Q a
evaluate x = everywhereM (return `extM` process)
           =<< qualifyNames (const True)
           =<< resolveFixities x
 where
  process e = eval . dsParens <$> dsInfix e

eval :: Exp -> Exp
eval e = case e of
  [e'| {{f}} {{ (pat -> Just x) }} {{ (pat -> Just y) }} |] -> case f of
    [e'| (+) |] -> build $ x + y
    [e'| (-) |] -> build $ x - y
    [e'| (*) |] -> build $ x * y
    _           -> e
  [e'| negate {{ (pat -> Just x) }} |] -> build $ negate x
  -- TODO: use de-sugaring for this
  [e'| ( {{x}} ) |] -> x
  _                 -> e
 where
  build = LitE . IntegerL
  pat (LitE (IntegerL x)) = Just x
  pat _                   = Nothing

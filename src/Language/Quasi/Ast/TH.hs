{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns, ScopedTypeVariables, TupleSections #-}

module Language.Quasi.Ast.TH
  (
  -- * Template Haskell AST Quoters
  -- $quoters
    e', p', t', d', cxt'

  -- * Template Haskell AST Quoters with convenience splices
  -- $quoters2
  , ec, pc, tc, dc, cxtc
  ) where


import Control.Applicative                ( (<$>) )
import Control.Arrow                      ( (***), second )
import Control.Monad.Trans.Class          ( lift )
import Control.Monad.Trans.Either         ( EitherT(..), hoistEither )
import Data.Either                        ( rights )
import Language.Haskell.TH
import Language.Haskell.TH.Quote          ( QuasiQuoter(..) )
import Language.Haskell.TH.Substitution   ( Free, Subst )
import Language.Quasi.Ast                 ( parseAstQQ )
import Language.Quasi.Internal.Conversion ( expToPat', expToPatMap )
import Language.Quasi.Internal.Utils
  ( ErrorQ, parseExp, parsePat, parseType, parseDecs, parseCxt, qualifyNames )
import qualified Language.Haskell.TH.Lift as TH
import qualified Data.Map as M
--import Language.Haskell.Exts.Me

import Text.Themplates

e', ec, p', pc, t', tc, d', dc, cxt', cxtc :: QuasiQuoter
e' = astQuoter False "Exp"  parseExp
ec = astQuoter True  "Exp"  parseExp

p' = astQuoter False "Pat"  parsePat
pc = astQuoter True  "Pat"  parsePat

t' = astQuoter False "Type" parseType
tc = astQuoter True  "Type" parseType

d' = astQuoter False "Decs" parseDecs
dc = astQuoter True  "Decs" parseDecs

cxt' = astQuoter False "Cxt" parseCxt
cxtc = astQuoter True  "Cxt" parseCxt

-- TODO: figure out how to conditionally qualify identifiers, for more readable
-- ddump-splices


-- | Builds a quoter for an AST, given a few configurations.  This is the function that
--   is used to implement @e'@, @p'@, @t'@, and @d'@.
astQuoter :: forall a. (TH.Lift a, Free a, Subst a)
          => Bool
          -> String
          -> (String -> Either String a)
          -> QuasiQuoter
astQuoter use_conv name parser = QuasiQuoter expr pat undefined undefined
 where
  errEitherT e = either fail return =<< runEitherT e

  parse_err :: String -> Either String b -> ErrorQ b
  parse_err err = hoistEither . mapLeft (err ++)

  parse_ast :: (Subst b, Free b)
            => (b -> Q c) -> ErrorQ b -> ErrorQ c
  parse_ast lifter p
    =   lift . lifter
    -- =<< lift . qualifyNames (const True)
    =<< p

  parse_splice :: (forall b. TH.Lift b => b -> Q c)
               -> (Exp -> c -> c)
               -> (String -> ErrorQ c)
               -> (String, String) -> ErrorQ [(c, c)]
  parse_splice lifter conv p (pun, code) = do
    parsed <- p code
    case results' of
      [] -> hoistEither
          $ Left $  "Parse error in fancy splice's pun expression: "
                 ++ pun ++ "\nHere are the different parse errors:\n"
                 ++ unlines (map (\(Left e) -> e) results)
      _  -> lift $ sequence [ (\r' -> (conv (conv_var v) r', parsed)) <$> r
                            | (v, r) <- results']
   where
    results' = rights results
    results = [ ("toExp" ,) . lifter <$> parseExp  pun
              , ("toPat" ,) . lifter <$> parsePat  pun
              , ("toType",) . lifter <$> parseType pun
              ] -- NOTE: this uses the regular Either monad.

    conv_var = VarE . mkName . ("Language.Haskell.TH.Convenience." ++)


  expr :: String -> Q Exp
  expr input = errEitherT $ do
    (chunks, free) <- parseAstQQ input
    spliced <- substSplices fst ast_p splice_p chunks
    return $ if null free
             then spliced
             else LamE (map (VarP . mkName) free) spliced
   where
    ast_p    = parse_ast TH.lift
             . parse_err ("Parse error in pun-substituted " ++ name ++ " AST literal")
             . parser
    splice_p = parse_splice TH.lift (\v e -> if use_conv then AppE v e else e)
             ( parse_err ("Parse error in " ++ name ++ " AST literal splice")
             . parseExp
             )

  pat :: String -> Q Pat
  pat input = errEitherT $ do
    (chunks, _) <- parseAstQQ input
    substSplices fst ast_p splice_p chunks
   where
    ast_p    = parse_ast lifter
             . parse_err ("Parse error in pun-substituted " ++ name ++ " AST match")
             . parser
    splice_p = parse_splice lifter (\v p -> if use_conv then ViewP v p else p)
             ( parse_err ("Parse error in " ++ name ++ " AST match splice")
             . parsePat
             )
    lifter x = pat_exp <$> TH.lift x
    pat_exp = expToPatMap . M.fromList
            . map (second (\n [p] -> ViewP (VarE . mkName $ n) $ expToPat' p))
            $  prefix_all "Language.Haskell.TH.Syntax."
                 [ ("mkOccName", "occString"), ("mkPkgName", "pkgString"), ("mkModName", "modString")]
            ++ prefix_all "Language.Quasi.Convenience."
                 [ ("toExp", "fromExp"), ("toPat", "fromPat"), ("toType", "fromType") ]
    prefix_all pre = map ((pre++) *** (pre++))
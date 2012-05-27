{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns, ScopedTypeVariables, TupleSections #-}

module Language.Haskell.TH.Builders where

import Control.Applicative              ( (<$>) )
import Control.Arrow                    ( (***) )
import Control.Monad.Trans.Class        ( lift )
import Control.Monad.Trans.Either       ( EitherT(..), hoistEither )
import Data.Char                        ( isSpace )
import Data.Either                      ( rights )
import Data.List                        ( isPrefixOf, isInfixOf, tails, partition )
import Data.Generics                    ( Data )
import Language.Haskell.TH
import Language.Haskell.TH.Convenience
import Language.Haskell.TH.Conversion   ( expToPat', expToPatMap )
import Language.Haskell.TH.Quote        ( QuasiQuoter(..) )
import Language.Haskell.TH.Substitution ( Free, Subst )
import Language.Haskell.TH.Syntax       ( occString, pkgString, modString )
import Language.Haskell.TH.Quote.Utils  ( parseExp, parsePat, parseType, parseDecs, qualifyNames, debug )
import qualified Language.Haskell.TH.Lift as TH
import qualified Data.Map as M

import Text.Themplates

e', p', t', d':: QuasiQuoter
e' = astQuoter False "Exp"  parseExp
p' = astQuoter False "Pat"  parsePat
t' = astQuoter False "Type" parseType
d' = astQuoter False "Decs" parseDecs

ec, pc, tc, dc :: QuasiQuoter
ec = astQuoter True "Exp"  parseExp
pc = astQuoter True "Pat"  parsePat
tc = astQuoter True "Type" parseType
dc = astQuoter True "Decs" parseDecs

astQuoter :: forall a. (TH.Lift a, Free a, Subst a)
          => Bool
          -> String
          -> (String -> Either String a)
          -> QuasiQuoter
astQuoter use_conv name parser = QuasiQuoter expr pat undefined undefined
 where
  errEitherT e = either fail return =<< runEitherT e

  parseErr :: String -> Either String b -> EitherT String Q b
  parseErr err = hoistEither . mapLeft (err ++)

  expr input = errEitherT $ do
    (chunks, free) <- parse input
    spliced <- doSplices ast_p splice_p return splice_t chunks
    return $ handle_free (map (\(Splice (x, _)) -> VarP $ mkName x) free) spliced
   where
    ast_p    = parseErr ("Parse error in pun-substituted " ++ name ++ " AST literal")
             . parser
    splice_p = parseErr ("Parse error in " ++ name ++ " AST literal splice")
             . parseExp
    handle_free [] = id
    handle_free xs = LamE xs
    splice_t v e = if use_conv then AppE  v e else e

  pat input = errEitherT $ do
    (chunks, _) <- parse input
    doSplices ast_p splice_p pat_exp splice_t chunks
   where
    ast_p    = parseErr ("Parse error in pun-substituted " ++ name ++ " AST match")
             . parser
    splice_p = parseErr ("Parse error in " ++ name ++ " AST match splice")
             . parsePat
    splice_t v p = if use_conv then ViewP v p else p
    pat_exp = expToPatMap . M.fromList
            $ map ( ("Language.Haskell.TH."++)
                *** (\e [p] -> viewP e $ expToPat' p) )
            [ ("Syntax.mkOccName",   [| occString |])
            , ("Syntax.mkPkgName",   [| pkgString |])
            , ("Syntax.mkModName",   [| modString |])
            , ("Convenience.toExp",  [| fromExp   |])
            , ("Convenience.toPat",  [| fromPat   |])
            , ("Convenience.toType", [| fromType  |])
            ]

  parse input = do
    chunks <- hoistEither $ parseSplices curlySplice input
    let avoids = filter ("splice" `isPrefixOf`) . tails
               $ concat [str | Chunk str <- chunks]

        named = zipWith give_names (unused_names avoids) chunks

    return (map process_free named, filter is_free named)
   where
    unused_names avoids
      = filter (\s -> not $ any (s `isPrefixOf`) avoids)
      $ map (("splice" ++) . show) ([1..] :: [Int])

    give_names n (Splice (Nothing, s)) = Splice (n, s)
    give_names _ (Splice (Just n, s))  = Splice (n, s)
    give_names _ (Chunk str)           = Chunk str

    is_free = chunk (const False) (null . filter (not . isSpace) . snd)

    process_free s@(Splice (n, _))
      | is_free s = Splice (n, n)
      | otherwise = s
    process_free x = x



doSplices :: forall a b. (Subst a, TH.Lift a, Free a, Data b, Ord b)
          => (String -> EitherT String Q a) -> (String -> EitherT String Q b) -- Parsers
          -> (Exp -> Q b) -> (Exp -> b -> b)                                  -- Results processors
          -> [Chunk Char (String, String)] -> EitherT String Q b              -- Resulting Chunks-processor.
doSplices ast_p splice_p convert splice_t
  = substSplices fst ast_parser splice_parser
 where
  ast_parser :: String -> EitherT String Q b
  ast_parser ts =   lift . literalize
                =<< lift . qualifyNames (const True)
                =<< ast_p ts

  splice_parser (pun, code) = do
    expr <- splice_p code
    case results' of
      [] -> hoistEither
          $ Left $  "Parse error in fancy splice's pun expression: "
                 ++ pun ++ "\nHere are the different parse errors:\n"
                 ++ unlines (map (\(Left e) -> e) results)
      _  -> lift $ sequence [(\r' -> (splice_t (conv_var v) r', expr)) <$> r | (v, r) <- results']
   where
    results = [ ("toExp" ,) . literalize <$> parseExp  pun
              , ("toPat" ,) . literalize <$> parsePat  pun
              , ("toType",) . literalize <$> parseType pun
              ] -- NOTE: this uses the regular Either monad.
    results' = rights results

  literalize :: TH.Lift c => c -> Q b
  literalize = (convert =<<) . TH.lift

  conv_var = VarE . mkName . ("Language.Haskell.TH.Convenience." ++)


-- This function takes a parser and replicating pattern, and infers the
-- inductive structure used to convert to a list.
{-
parserListCons
  :: (Monad m, Data a)
  => (String -> EitherT String m a)
  -> (String, String, String)
  -> a 

parserListPat
  :: 
-}
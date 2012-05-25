{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns, ScopedTypeVariables, TupleSections #-}

module Language.Haskell.TH.Builders where

import Control.Applicative              ( (<$>) )
import Control.Arrow                    ( (***) )
import Control.Monad.Trans.Class        ( lift )
import Control.Monad.Trans.Either       ( EitherT(..), hoistEither )
import Data.Either                      ( rights )
import Data.List                        ( isPrefixOf, isInfixOf, tails )
import Data.Generics                    ( Data )
import Language.Haskell.TH
import Language.Haskell.TH.Convenience
import Language.Haskell.TH.Conversion   ( expToPat', expToPatMap )
import Language.Haskell.TH.Quote        ( QuasiQuoter(..) )
import Language.Haskell.TH.Substitution ( Free, Subst )
import Language.Haskell.TH.Syntax       ( occString, pkgString, modString )
import Language.Haskell.TH.Quote.Utils  ( parseExp, parsePat, parseType, parseDecs, qualifyNames )
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
          -> [Char]
          -> ([Char] -> String -> Either String a)
          -> QuasiQuoter
astQuoter use_conv name parser = QuasiQuoter
  ( errEitherT . doSplices e_ast e_splice return  e_apply )
  ( errEitherT . doSplices p_ast p_splice pat_exp p_apply )
  undefined undefined
 where
  errEitherT e = either fail return =<< runEitherT e
  e_ast = hoistEither . parser ("Parse error in pun-substituted " ++ name ++ " AST literal")
  p_ast = hoistEither . parser ("Parse error in pun-substituted " ++ name ++ " AST match")
  e_splice s = hoistEither $ parseExp ("Parse error in " ++ name ++ " AST literal splice") s
  p_splice s = hoistEither . parsePat ("Parse error in " ++ name ++ " AST match splice")
                           $ "(" ++ s ++ ")"
  e_apply v e = if use_conv then AppE  v e else e
  p_apply v p = if use_conv then ViewP v p else p
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

doSplices :: forall a b. (Subst a, TH.Lift a, Free a, Data b, Ord b)
          => (String -> EitherT String Q a)
          -> (String -> EitherT String Q b)
          -> (Exp -> Q b)
          -> (Exp -> b -> b)
          -> String
          -> EitherT String Q b
doSplices ast_p splice_p convert post_apply input = do
  chunks <- hoistEither $ parseSplices curlySplice input
  
  let avoids = filter ("splice" `isPrefixOf`) . tails
             $ concat [str | Chunk str <- chunks]
      named = zipWith give_names (unused_names avoids) chunks
  
  substSplices fst ast_parser splice_parser named
 where
  unused_names avoids
    = filter (\s -> not $ any (s `isPrefixOf`) avoids)
    $ map (("splice" ++) . show) ([1..] :: [Int])
 
  give_names n (Splice (Nothing, s)) = Splice (n, s)
  give_names _ (Splice (Just n, s))  = Splice (n, s)
  give_names _ (Chunk str)           = Chunk str

  -- TODO: (not . (`elem` sub_names))

  ast_parser :: String -> EitherT String Q b
  ast_parser ts = lift . literalize =<< lift . qualifyNames (const True) =<< ast_p ts

  splice_parser (pun, code) = do
    expr <- splice_p code
    case results' of
      [] -> hoistEither $ Left $ "Parse error in fancy splice's pun expression: " ++ pun ++ "\n"
                              ++ "Here are the different parse errors:\n"
                              ++ unlines (map (\(Left e) -> e) results)
      _  -> sequence [(\r' -> (post_apply (conv_var v) r', expr)) <$> lift r | (v, r) <- results']
   where
    results = [ ("toExp" ,) . literalize <$> parseExp  "" pun
              , ("toPat" ,) . literalize <$> parsePat  "" pun
              , ("toType",) . literalize <$> parseType "" pun
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
{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns, ScopedTypeVariables, TupleSections #-}

module Language.Haskell.TH.Builders where

import qualified Data.Map as M

import Control.Applicative              ( (<$>), (<*>) )
import Control.Arrow                    ( (&&&), (***), first, second )
import Control.Monad.Trans.Class        ( lift )
import Control.Monad.Trans.Either       ( EitherT(..), hoistEither )
import Data.Char                        ( toUpper )
import Data.Either                      ( rights )
import Data.List                        ( isPrefixOf, isInfixOf, tails )
import Data.Maybe                       ( catMaybes )
import Data.Generics                    ( Data, gmapQ, extT, everywhere )
import Debug.Trace (trace)
import qualified Language.Haskell.Exts as Exts
import qualified Language.Haskell.Meta as Exts
import qualified Language.Haskell.TH.Lift as TH
import Language.Haskell.TH
import Language.Haskell.TH.Convenience
import Language.Haskell.TH.Conversion   ( expToPat', expToPatMap )
import Language.Haskell.TH.Named        ( name_of )
import Language.Haskell.TH.Quote        ( QuasiQuoter(..) )
import Language.Haskell.TH.Substitution ( Free(..), Subst(..), Sub(..) )
import Language.Haskell.TH.Syntax       ( occString, pkgString, modString )

import Text.Themplates

import Text.Parsec                      ( string, char, lookAhead, anyChar, manyTill, try, parse )

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

astQuoter use_conv name parser = QuasiQuoter
  ( errEitherT . doSplices e_ast e_splice return  e_apply )
  ( errEitherT . doSplices p_ast p_splice pat_exp p_apply )
  undefined undefined
 where
  errEitherT e = either fail return =<< runEitherT e
  e_ast = hoistEither . parser ("Parse error in pun-substituted " ++ name ++ " AST literal")
  p_ast = hoistEither . parser ("Parse error in pun-substituted " ++ name ++ " AST match")
  e_splice s = hoistEither $ parseExp ("Parse error in " ++ name ++ " AST literal splice") s
  p_splice s = hoistEither $ parsePat ("Parse error in " ++ name ++ " AST match splice")   s
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

data LitSplice = PlainSplice String | FancySplice String String

fancySplice code_parser = do
  c <- lookAhead anyChar
  case c of
    '<' -> do
      char '<'
      splice <- manyTill (escapeParser '\\' [('>', '>'), ('\\', '\\')])
                         (char '>')
      code <- code_parser
      return $ FancySplice splice code
    _ ->  do
      code <- code_parser
      return $ PlainSplice code


oldParser = spliceParser $ do
  string "$(" 
  fancySplice (concat <$> nestParser (delimParser '(' ')') 
                                     [try $ char ')' >> return ""])

newParser = spliceParser $ do
  string "{{" 
  fancySplice (concat <$> nestParser (delimParser '{' '}') 
                                     [try $ string "}}" >> return ""])


doSplices :: forall a b. (Subst a, TH.Lift a, Free a, Data b, Ord b)
          => (String -> EitherT String Q a)
          -> (String -> EitherT String Q b)
          -> (Exp -> Q b)
          -> (Exp -> b -> b)
          -> String
          -> EitherT String Q b
doSplices ast_p splice_p convert post_apply input = do
  chunks <- hoistEither . mapLeft show $ parse newParser "" input
  
  let avoids = filter ("splice" `isPrefixOf`) . tails
             $ concat [str | Chunk str <- chunks]
      named = zipWith give_names (unused_names avoids) chunks
  
  substSplices fst ast_parser splice_parser named
 where
  unused_names avoids
    = filter (\s -> not $ any (s `isPrefixOf`) avoids)
    $ map (("splice" ++) . show) ([1..] :: [Int])
 
  give_names n (Splice (PlainSplice   s)) = Splice (n, s)
  give_names _ (Splice (FancySplice n s)) = Splice (n, s)
  give_names _ (Chunk str)                = Chunk str

  -- TODO: (not . (`elem` sub_names))

  ast_parser :: String -> EitherT String Q b
  ast_parser ts = lift . literalize =<< lift . qualify_names (const True) =<< ast_p ts

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

  qualify_names :: (Free c, Subst c) => (Name -> Bool) -> c -> Q c
  qualify_names f x = do
    let vars = rights $ free x
    infos <- mapM (\n -> ((n,) <$>) <$> recover (return Nothing) (Just <$> reify n)) vars
    let rewrites = M.fromList . map (second name_of) . filter (f . fst) $ catMaybes infos
    return $ subst (Sub rewrites M.empty M.empty) x

  conv_var = VarE . mkName . ("Language.Haskell.TH.Convenience." ++)


-- Misc Utils

debug :: Show x => x -> x
debug x = trace (show x) x

mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapEither f g = either (Left . f) (Right . g)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft  f = mapEither f id

mapRight :: (b -> d) -> Either a b -> Either a d
mapRight f = mapEither id f

parseExp  :: String -> String -> Either String Exp
parseExp  err s = mapEither ((err++":\n"++s)++) Exts.toExp  . Exts.parseResultToEither
                $ Exts.parseExpWithMode  parseMode s
parsePat  :: String -> String -> Either String Pat
parsePat  err s = mapEither ((err++":\n"++s)++) Exts.toPat  . Exts.parseResultToEither
                $ Exts.parsePatWithMode  parseMode s
parseType :: String -> String -> Either String Type
parseType err s = mapEither ((err++":\n"++s)++) Exts.toType . Exts.parseResultToEither
                $ Exts.parseTypeWithMode parseMode s
parseDecs :: String -> String -> Either String [Dec]
parseDecs err s = mapEither ((err++":\n"++s)++) 
                            (\(Exts.Module _ _ _ _ _ _ x) -> Exts.toDecs x)
                . Exts.parseResultToEither
                $ Exts.parseModuleWithMode parseMode s

-- | Parse mode with all extensions and no fixities.
parseMode :: Exts.ParseMode
parseMode = Exts.ParseMode
  { Exts.parseFilename = ""
  , Exts.extensions = Exts.glasgowExts ++ [Exts.TupleSections, Exts.BangPatterns, Exts.ViewPatterns]
  , Exts.ignoreLinePragmas = False
  , Exts.ignoreLanguagePragmas = False
  , Exts.fixities = Nothing
  }
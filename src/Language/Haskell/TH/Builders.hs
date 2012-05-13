{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns, ScopedTypeVariables, TupleSections #-}

module Language.Haskell.TH.Builders where

import qualified Data.Map as M

import Control.Applicative              ( (<$>), (<*>) )
import Control.Arrow                    ( (&&&), (***), first, second )
import Data.Char                        ( toUpper )
import Data.Data                        ( Data, gmapQ )
import Data.Either                      ( rights )
import Data.List                        ( isPrefixOf, isInfixOf, tails )
import Data.Maybe                       ( catMaybes )
import Data.Generics.Aliases            ( extM, extQ )
import Data.Generics.Schemes            ( everywhereM )
import Debug.Trace (trace)
import qualified Language.Haskell.Exts as Exts
import qualified Language.Haskell.Meta as Exts 
  ( toExp, toPat, toType, toDecs, parseResultToEither )
import Language.Haskell.TH
import Language.Haskell.TH.Convenience
import Language.Haskell.TH.Conversion   ( expToPat', expToPatMap )
import Language.Haskell.TH.Lift         ( Lift, lift )
import Language.Haskell.TH.Named        ( name_of )
import Language.Haskell.TH.Quote        ( QuasiQuoter(..) )
import Language.Haskell.TH.Substitution ( Free(..), Subst(..), Sub(..) )
import Language.Haskell.TH.Syntax       ( occString, pkgString, modString )
import Text.Parsec
  ( getInput, lookAhead, manyTill, between, string, oneOf, eof, noneOf, char, anyChar, parse, (<|>) )


thExp, thPat, thType, thDecs :: QuasiQuoter
thExp  = astQuoter False "Exp"  parseExp
thPat  = astQuoter False "Pat"  parsePat
thType = astQuoter False "Type" parseType
thDecs = astQuoter False "Decs" parseDecs

thExp', thPat', thType', thDecs' :: QuasiQuoter
thExp'  = astQuoter True "Exp"  parseExp
thPat'  = astQuoter True "Pat"  parsePat
thType' = astQuoter True "Type" parseType
thDecs' = astQuoter True "Decs" parseDecs


astQuoter use_conv name parser = QuasiQuoter
  (either error id . doSplices e_ast e_splice return  e_apply)
  (either error id . doSplices p_ast p_splice pat_exp p_apply)
  undefined undefined
 where
  e_ast = parser $ "Parse error in pun-substituted " ++ name ++ " AST literal"
  p_ast = parser $ "Parse error in pun-substituted " ++ name ++ " AST match"
  e_splice s = parseExp ("Parse error in " ++ name ++ " AST literal splice") s
  p_splice s = parsePat ("Parse error in " ++ name ++ " AST match splice")   s
  e_apply n e = return $ if use_conv then (AppE  $ VarE n) e else e
  p_apply n p = return $ if use_conv then (ViewP $ VarE n) p else p
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

doSplices :: forall a b. (Subst a, Lift a, Free a, Data b, Ord b, Ppr b, Ppr a)
          => (String -> Either String a)
          -> (String -> Either String b)
          -> (Exp -> Q b)
          -> (Name -> b -> Q b)
          -> String
          -> Either String (Q b)
doSplices ast_parser splice_parser from_exp apply_conv input = do
  chunks <- parseSplices input
  
  let avoids = filter ("splice" `isPrefixOf`) . tails
             $ concat [str | ChunkLit str <- chunks]
      named = zipWith give_names (unused_names avoids) chunks
      code = concatMap (either id fst) named

  parsed <- ast_parser code

  subs <- mapM (secondM splice_parser) $ rights named

  let sub_names = concatMap get_names subs
      puns :: Lift c => (String -> String -> Either String c) -> (b -> Q b) -> Q [(b, b)]
      puns f g = mapM (bothM literalize g) . rights $ map (firstM $ f "") subs

  return $ do
    qualified <- qualify_names (not . (`elem` sub_names)) parsed
    literalized <- literalize qualified
    pes <- puns parseExp  (apply_conv $ conv_var "toExp")
    pps <- puns parsePat  (apply_conv $ conv_var "toPat")
    pts <- puns parseType (apply_conv $ conv_var "toType")
    substitute (M.fromList $ pes ++ pps ++ pts) literalized
 where
  unused_names avoids
    = filter (\s -> not $ any (s `isPrefixOf`) avoids)
    $ map (("splice" ++) . show) [1..]

  give_names n (ChunkSplice (PlainSplice   s)) = Right (n, s)
  give_names _ (ChunkSplice (FancySplice n s)) = Right (n, s)
  give_names _ (ChunkLit str)                  = Left str

  get_names :: Data c => c -> [Name]
  get_names = concat . gmapQ (get_names `extQ` ((:[]) :: Name -> [Name]))

  qualify_names :: (Free a, Subst a) => (Name -> Bool) -> a -> Q a
  qualify_names f x = do
    let vars = rights $ free x
    infos <- mapM (\n -> ((n,) <$>) <$> recover (return Nothing) (Just <$> reify n)) vars
    let rewrites = M.fromList . map (second name_of) . filter (f . fst) $ catMaybes infos
    return $ subst (Sub rewrites M.empty M.empty) x

  conv_var = mkName . ("Language.Haskell.TH.Convenience." ++)

  literalize x = from_exp =<< lift x

  substitute :: M.Map b b -> b -> Q b
  substitute m = everywhereM (return `extM` do_sub)
   where
    do_sub :: b -> Q b
    do_sub e | Just r <- M.lookup e m = return r
             | otherwise              = return e


data Splice = PlainSplice String | FancySplice String String
  deriving Show

data Chunk = ChunkSplice Splice | ChunkLit String
  deriving Show

-- TODO: handle nested quoters

parseSplices  :: String -> Either String [Chunk]
parseSplices  = mapEither show consolidate . parse parser ""
 where
  consolidate [] = []
  consolidate (ChunkLit ""             : xs) = consolidate xs
  consolidate (ChunkLit x : ChunkLit y : xs) = consolidate $ ChunkLit (x ++ y) : xs
  consolidate (x:xs)                         = x : consolidate xs
  -- This is probably awful parsec style.
  parser = do
    prefix <- manyTill anyChar 
            $ lookAhead eof <|> (lookAhead (oneOf "$()") >> return ())
    (lookAhead eof >> return [ChunkLit prefix])
     <|> do
      a <- lookAhead anyChar
      (ChunkLit prefix:) <$> case a of

        '(' -> do
          char '('
          nested <- parser
          char ')'
          continue $ [ChunkLit "("] ++ nested ++ [ChunkLit ")"]

        '$' -> do
          char '$'
          b <- anyChar
          case b of

            '(' -> do
              c <- lookAhead anyChar
              case c of

                '<' -> do
                  char '<'
                  splice <- fancy_escaped
                  char '>'
                  code <- splice_code 0
                  continue [ChunkSplice $ FancySplice splice code]

                _ ->  do
                  code <- splice_code 0
                  continue [ChunkSplice $ PlainSplice code]

            _ -> (ChunkLit "$":) <$> parser

        ')' -> return []

        _   -> fail "Impossible!"

  continue cs = (cs ++) <$> parser

  fancy_escaped = (string "\\>" >> fancy_escaped >>= return . ('>':))
              <|> ((:) <$> noneOf ">" <*> fancy_escaped)
              <|> return []

  parens p = (++")") . ('(':) <$> between (char '(') (char ')') p

  splice_code n = do
    a <- anyChar
    case a of
      '(' ->                               (a:) <$> splice_code (n + 1)
      ')' -> if n == 0 then return "" else (a:) <$> splice_code (n - 1)
      _   ->                               (a:) <$> splice_code n


-- Misc Utils

debug x = trace (show x) x

mapEither f g = either (Left . f) (Right . g)

firstM  f (x, y) = f x >>= \x' ->                return (x', y )

secondM g (x, y) =                g y >>= \y' -> return (x , y')

bothM f g (x, y) = f x >>= \x' -> g y >>= \y' -> return (x', y')

-- Haskell Src Exts Utils

parseExp  err s = mapEither ((err++":\n"++s)++) Exts.toExp  . Exts.parseResultToEither
                $ Exts.parseExpWithMode  parseMode s
parsePat  err s = mapEither ((err++":\n"++s)++) Exts.toPat  . Exts.parseResultToEither
                $ Exts.parsePatWithMode  parseMode s
parseType err s = mapEither ((err++":\n"++s)++) Exts.toType . Exts.parseResultToEither
                $ Exts.parseTypeWithMode parseMode s
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
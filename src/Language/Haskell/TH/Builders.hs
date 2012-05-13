{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns, ScopedTypeVariables #-}

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
import Language.Haskell.Exts
  ( ParseMode(..), ParseResult(..), Module(..), Decl, Extension(..), SrcLoc(..)
  , glasgowExts
  , parseExpWithMode, parsePatWithMode, parseTypeWithMode, parseModuleWithMode )

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
thExp  = astQuoter "Exp"  parseExp  id id
thPat  = astQuoter "Pat"  parsePat  id id
thType = astQuoter "Type" parseType id id
thDecs = astQuoter "Decs" parseDecs id id

thExp', thPat', thType', thDecs' :: QuasiQuoter
thExp'  = astQuoter "Exp"  parseExp  (AppE $ converter "toExp" ) (ViewP $ converter "fromExp" )
thPat'  = astQuoter "Pat"  parsePat  (AppE $ converter "toPat" ) (ViewP $ converter "fromPat" )
thType' = astQuoter "Type" parseType (AppE $ converter "toType") (ViewP $ converter "fromType")
thDecs' = astQuoter "Decs" parseDecs (AppE $ converter "toDec" ) (ViewP $ converter "fromDec" )

converter = VarE . mkName . ("Language.Haskell.TH.Convenience." ++)

astQuoter name parser e_splicer p_splicer = QuasiQuoter
  (either error id . doSplices e_ast_parser e_splice_parser return)
  (either error id . doSplices p_ast_parser p_splice_parser pat_exp)
  undefined undefined
 where
  e_ast_parser = parser $ "Parse error in pun-substituted " ++ name ++ " AST literal"
  p_ast_parser = parser $ "Parse error in pun-substituted " ++ name ++ " AST match"
  e_splice_parser s = e_splicer <$> parseExp ("Parse error in " ++ name ++ " AST literal splice") s
  p_splice_parser s = p_splicer <$> parsePat ("Parse error in " ++ name ++ " AST match splice")   s
  pat_exp = expToPatMap . M.fromList
          $ map ( ("Language.Haskell.TH."++)
              *** (\e [p] -> viewP e $ expToPat' p) )
          [ ("Syntax.mkOccName",   [| occString |])
          , ("Syntax.mkPkgName",   [| pkgString |])
          , ("Syntax.mkModName",   [| modString |])
          ]

doSplices :: forall a b. (Subst a, Lift a, Free a, Data b, Ord b)
           => (String -> Either String a)
           -> (String -> Either String b)
           -> (Exp -> Q b)
           -> String
           -> Either String (Q b)
doSplices ast_parser splice_parser convert input = do
  chunks <- parseSplices input
  
  let avoids = filter ("splice" `isPrefixOf`) . tails
             $ concat [str | ChunkLit str <- chunks]
      named = zipWith give_names (unused_names avoids) chunks
      code = concatMap (either id fst) named

  parsed <- ast_parser code

  subs <- mapM (secondM parse_sub) $ rights named

  let sub_names = concatMap get_names subs
      puns :: Lift c => (String -> String -> Either String c) -> Q [(b, b)]
      puns f = mapM (firstM literalize) . rights $ map (firstM $ f "") subs

  return $ do
    qualified <- qualify_names (not . (`elem` sub_names)) parsed
    literalized <- literalize qualified
    pes <- puns parseExp
    pps <- puns parsePat
    pts <- puns parseType
    substitute (M.fromList $ pes ++ pps ++ pts) literalized
 where
  unused_names avoids
    = filter (\s -> not $ any (s `isPrefixOf`) avoids)
    $ map (("splice" ++) . show) [1..]

  parse_sub :: String -> Either String b
  parse_sub v
    = mapEither (\e -> "Parse error in splice:\n" ++ v ++ "\n" ++ e) id
    $ splice_parser v

  give_names n (ChunkSplice (PlainSplice   s)) = Right (n, s)
  give_names _ (ChunkSplice (FancySplice n s)) = Right (n, s)
  give_names _ (ChunkLit str)                  = Left str

  get_names :: Data c => c -> [Name]
  get_names = concat . gmapQ (get_names `extQ` ((:[]) :: Name -> [Name]))

  qualify_names :: (Free a, Subst a) => (Name -> Bool) -> a -> Q a
  qualify_names f x = do
    let vars = rights $ free x
    infos <- mapM (\n -> recover (return Nothing) (Just <$> reify n))
           $ filter f vars
    let rewrites = M.fromList . zip vars . map name_of $ catMaybes infos
    return $ subst (Sub rewrites M.empty M.empty) x

  literalize :: Lift c => c -> Q b
  literalize x = convert =<< lift x

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
  consolidate (ChunkLit "" : xs)             = consolidate xs
  consolidate (ChunkLit x : ChunkLit y : xs) = consolidate $ ChunkLit (x ++ y) : xs
  consolidate (x:xs)                         = x : consolidate xs
  -- This is probably not good parsec style.
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
              c <- anyChar
              case c of
                '<' -> do
                  splice <- fancy_escaped
                  char '>'
                  code <- splice_code
                  char ')'
                  continue [ChunkSplice $ FancySplice splice code]
                _ ->  do
                  splice <- splice_code
                  char ')'
                  continue [ChunkSplice $ PlainSplice (c:splice)]
            _ -> (ChunkLit "$":) <$> parser
        ')' -> return [ChunkLit ""]
        _   -> fail "Impossible!"

  continue cs = (cs ++) <$> parser

  fancy_escaped = (string "\\>" >> fancy_escaped >>= return . ('>':))
              <|> ((:) <$> noneOf ">" <*> fancy_escaped)
              <|> return []

  parens p = (++")") . ('(':) <$> between (char '(') (char ')') p

  splice_code = parens splice_code
            <|> ((:) <$> noneOf "()" <*> splice_code)
            <|> return []


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
parseDecs err s = mapEither ((err++":\n"++s)++) (\(Module _ _ _ _ _ _ x) -> Exts.toDecs x)
                . Exts.parseResultToEither
                $ Exts.parseModuleWithMode parseMode s

-- | Parse mode with all extensions and no fixities.
parseMode :: Exts.ParseMode
parseMode = Exts.ParseMode
  { parseFilename = ""
  , extensions = Exts.glasgowExts ++ [Exts.TupleSections, Exts.BangPatterns, Exts.ViewPatterns]
  , ignoreLinePragmas = False
  , ignoreLanguagePragmas = False
  , fixities = Nothing
  }
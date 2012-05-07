{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Language.Haskell.TH.Builders where

import qualified Data.Map as M

import Control.Applicative              ( (<$>) )
import Control.Arrow                    ( (&&&) )
import Data.Either                      ( rights )
import Language.Haskell.Exts
  ( ParseMode(..), ParseResult, Module(..), Decl, Extension(..), glasgowExts
  , parseExpWithMode, parsePatWithMode, parseTypeWithMode, parseModuleWithMode )
import Language.Haskell.Meta ( toExp, toPat, toType, toDecs, parseResultToEither )
import Language.Haskell.TH
import Language.Haskell.TH.Conversion   ( expToPat, expToPat' )
import Language.Haskell.TH.Lift         ( lift )
import Language.Haskell.TH.Named        ( name_of )
import Language.Haskell.TH.Quote        ( QuasiQuoter(..) )
import Language.Haskell.TH.Substitution ( Free(..), Subst(..), Sub(..) )
import Language.Haskell.TH.Syntax       ( occString, pkgString, modString )

--TODO: th-extra is nice!
--TODO: use th-build?

--TODO: there are probably bugs here.

qualifyNames :: (Free a, Subst a) => a -> Q a
qualifyNames x = do
  let vars = rights $ free x
  infos <- mapM reify vars
--  runIO $ print infos
  let rewrites = M.fromList . zip vars $ map name_of infos
  return $ subst (Sub rewrites M.empty M.empty) x

parse = rec 0 Nothing ""
 where
  l = Left  . reverse
  r = Right . reverse
  rec n s a txt
    = case (txt, s) of
        (        [], Nothing)           -> [l a]
        (    '(':xs,       _)           ->       rec (n + 1)              s ( "(" ++ a) xs
        ('$':'(':xs, Nothing)           -> l a : rec (n + 1) (Just $ n + 1) ""          xs
        (    ')':xs, Just n') | n' == n -> r a : rec (n - 1)        Nothing ""          xs
        (    ')':xs,       _)           ->       rec (n - 1)              s ( ")" ++ a) xs
        (      c:xs,       _)           ->       rec n                    s ( [c] ++ a) xs


{-
thExp = QuasiQuoter expr undefined undefined undefined
 where
  expr = resolve . build names . parse
    let e = parseExpWithMode parseMode

    lift =<< qualifyNames e
   where
    names = filter (`notElem` s) $ map (("splice" ++) . show) [1..]

    build    ns  (Left  s:xs) = Left     s  : build ns xs
    build (n:ns) (Right x:xs) = Right (x,n) : build ns xs

    resolve = case attempt $ SrcLoc "" 1 1 of
      (locs, str) -> case parseExpWithMode parseMode of

      (locs, )
    
    attempt l = (map fst &&& reverse . snd . last)
              . scanl (\(l, a) s -> (uloc l s, reverse s ++ a)) (SrcLoc "" 1 1, "") 
              . map (either id snd)

    uloc (SrcLoc  f l c) t = SrcLoc f l' c'
     where
      l' = l + length (filter (=='\n') t)
      c' = length . takeWhile (/='\n') $ reverse t
-}


thExpr = QuasiQuoter expr pat undefined undefined
 where
  expr s = lift =<< qualifyNames (parseExp s)
  pat  s = do
    e <- expr s
    -- runIO $ print e
    expToPat func e

  func x@[VarE fn, p@(LitE (StringL n))]
    | pprint fn == "Language.Haskell.TH.Syntax.mkOccName"
    = viewP [| occString |] $ expToPat' p

    | pprint fn == "Language.Haskell.TH.Syntax.mkPkgName"
    = viewP [| pkgString |] $ expToPat' p

    | pprint fn == "Language.Haskell.TH.Syntax.mkModName"
    = viewP [| modString |] $ expToPat' p

    | otherwise = error $ "Could not convert function: " ++ pprint fn
  func x = error $ "Could not convert: " ++ show x

  -- undefined -- qualifyNames . parseExp

{-
thPat  = QuasiQuoter expr pat undefined undefined
 where
  expr = qualifyNames . parsePat
  pat  = qualifyNames . parsePat

thDecl = QuasiQuoter expr pat undefined undefined

thType = QuasiQuoter expr pat undefined undefined
-}


-- Haskell Src Exts Utils

parseExp  = toExp  . fromParseResult . parseExpWithMode  parseMode
parsePat  = toPat  . fromParseResult . parsePatWithMode  parseMode
parseType = toType . fromParseResult . parseTypeWithMode parseMode
parseDecs = toDecs . (\(Module _ _ _ _ _ _ x) -> x)
                   . fromParseResult . parseModuleWithMode parseMode

-- | Throws the @Left@ as an error, otherwise yields the @Right@ value.
fromError :: (Either String a) -> a
fromError = either error id

-- | Throws error on parse failure.
fromParseResult :: ParseResult a -> a
fromParseResult = fromError . parseResultToEither

-- | Parse mode with all extensions and no fixities.
parseMode :: ParseMode
parseMode = ParseMode
  { parseFilename = ""
  , extensions = glasgowExts ++ [TupleSections, BangPatterns]
  , ignoreLinePragmas = False
  , ignoreLanguagePragmas = False
  , fixities = Nothing
  }

{-
qualifyNames :: Data a => a -> Q a
qualifyNames = 
 where
  helper scope x = doDec `extM` doType `extM` doPat `extM` doExp
   where
    rewrite n
      | n `elem` scope = n
      | otherwise = reify n
    doExp (VarE n)
    doExp (ConE n)
    doExp (RecConE n fs) = if n `elem` scope then
-}

-- TODO: handle \ escaping
-- TODO: handle nesting



{-
parse = rec [("", "")]
 where 
  rec           [] [("", a)]     = 
  rec (    '"':xs)           st  = parse xs (("\"",    "") : st)
  rec ('$':'(':xs)           st  = parse xs (("$(",    "") : st)
  rec (    '(':xs) (('"', a):st) = parse xs (( '"', '(':a) : st)
  rec (    '(':xs)           st  = parse xs (( '(',    "") : st)
  rec (    ')':xs) (("$(",a):st) = reverse a
  rec (    ')':xs) (("(", a):st) = reverse a
  rec (    ')':xs) ((  d, a):st) = Left $ "Expected match for " ++ d ++ " instead of ')'."
  rec (      c:xs) ((d,   a):st) = parse xs ((   d,   c:a) : st)
-}
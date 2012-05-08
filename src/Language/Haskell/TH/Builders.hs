{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}
module Language.Haskell.TH.Builders where

import qualified Data.Map as M

import Control.Applicative              ( (<$>) )
import Control.Arrow                    ( (&&&), second )
import Data.Char                        ( toUpper )
import Data.Data                        ( Data )
import Data.Either                      ( rights )
import Data.List                        ( isPrefixOf, isInfixOf )
import Data.Generics.Aliases            ( extM )
import Data.Generics.Schemes            ( everywhereM )
import Debug.Trace (trace)
import Language.Haskell.Exts
  ( ParseMode(..), ParseResult(..), Module(..), Decl, Extension(..), SrcLoc(..)
  , glasgowExts
  , parseExpWithMode, parsePatWithMode, parseTypeWithMode, parseModuleWithMode )

import qualified Language.Haskell.Exts as Exts
import Language.Haskell.Meta ( toExp, toPat, toType, toDecs, parseResultToEither )
import Language.Haskell.TH
import Language.Haskell.TH.Conversion   ( expToPat, expToPat' )
import Language.Haskell.TH.Lift         ( lift )
import Language.Haskell.TH.Named        ( name_of )
import Language.Haskell.TH.Quote        ( QuasiQuoter(..) )
import Language.Haskell.TH.Substitution ( Free(..), Subst(..), Sub(..) )
import Language.Haskell.TH.Syntax       ( occString, pkgString, modString )

debug x = trace (show x) x

--TODO: there are probably bugs here.

qualifyNames :: (Free a, Subst a) => a -> Q a
qualifyNames x = do
  let vars = rights $ free x
  infos <- mapM reify $ filter (\n -> not $ "splice" `isPrefixOf` nameBase n) vars
--  runIO $ print infos
  let rewrites = M.fromList . zip vars $ map name_of infos
  return $ subst (Sub rewrites M.empty M.empty) x

-- TODO: handle \ escaping
-- TODO: handle nesting

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


thExp = QuasiQuoter expr undefined undefined undefined
 where
  expr s = substitute 
         . second (\e -> lift =<< qualifyNames e)
         . resolve . build names $ parse s
   where
    substitute :: ([((String, Int), String)], ExpQ) -> ExpQ
    substitute (xs, expr) 
      = everywhereM (return `extM` doExp `extM` doTyp `extM` doPat) =<< expr
     where
      m = M.fromList xs
      doExp :: Exp -> ExpQ
      doExp e@(AppE (ConE (pprint -> "Language.Haskell.TH.Syntax.VarE"))
                    (AppE (AppE (ConE (pprint -> "Language.Haskell.TH.Syntax.Name"))
                                (AppE (VarE (pprint -> "Language.Haskell.TH.Syntax.mkOccName"))
                                      (LitE (StringL n))))
                          (ConE (pprint -> "Language.Haskell.TH.Syntax.NameS"))))
        | Just x <- M.lookup (n, 0) m = return $ parseExp x
        | otherwise = return e

      {-
      doExp v@(LitE (StringL n))
        | Just x <- M.lookup (n, 0) m = return $ parseExp x
        | otherwise = return v -}
      doExp e = return e
      doTyp :: Type -> TypeQ
      doTyp t = return t
      doPat :: Pat  -> PatQ
      doPat p = return p

    names :: [String]
    names = filter (not . flip isInfixOf s) $ map (("splice" ++) . show) [1..]

    build     _            [] = []
    build    ns  (Left  s:xs) = Left         s  : build ns xs
    build (n:ns) (Right x:xs) = Right ((n,0),x) : build ns xs

    resolve :: [Either String ((String, Int), String)] -> ([((String, Int), String)], Exp)
    resolve xs = case attempt (SrcLoc "" 1 1, [], "") xs of
      (locs, str) -> case parseExpWithMode parseMode str of
        (ParseOk x) -> (rights xs, toExp x)
        (ParseFailed l e) -> case break insideSplice $ zip xs (tail locs) of
          (good, (Right ((n, i), x), _) : rest)
            | i < 2     -> resolve $ map fst good ++ (Right ((n, i + 1), x) : map fst rest)
            | otherwise -> error $ "Could not find placeholder for splice '" ++ x ++ "'\n"
                                ++ "due to parse error: " ++ perror e
          _ -> error $ "Parse Error: " ++ perror e
         where
          insideSplice (Right _, loc) = loc >= l
          insideSplice _ = False
          perror e = e ++ "\nIn AST with splice placeholders:" ++ str

    attempt :: (SrcLoc, [SrcLoc], String) -> [Either String ((String, Int), String)]
            ->         ([SrcLoc], String)
    attempt (l, ls, a)     [] = (reverse ls, reverse a)
    attempt (l, ls, a) (x:xs) = rec $ either id (spliceDummy . fst) x
     where
      rec str = attempt (uloc l rev, l:ls, rev ++ a) xs
       where rev = reverse str

    spliceDummy (n, 0) = n
    spliceDummy (n, 1) = "_ -> " ++ n
    spliceDummy (n, 2) = "(" ++ capitalize n ++ " a)"

    capitalize (x:xs) = toUpper x : xs

    -- NOTE: only works for a reversed string.
    uloc (SrcLoc  f l c) t = SrcLoc f l'
                           $ if l /= l' then c' else c + c'
     where
      l' = l + length (filter (=='\n') t)
      c' = length $ takeWhile (/='\n') t


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
{-# LANGUAGE TemplateHaskell, QuasiQuotes, TupleSections, ConstraintKinds #-}

-- utility functions for debugging TH in GHCI
module Language.Quasi.Internal.Utils where

import Control.Applicative              ( (<$>)     )
import Control.Monad.Trans.Class        ( lift      )
import Control.Monad.Trans.Either       ( EitherT, hoistEither )
import Data.Generics                    ( Data, everywhere, extT )
import Data.Either                      ( rights    )
import Data.Maybe                       ( catMaybes )
import Debug.Trace                      ( trace     )
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Named        ( Named(..) )
import Language.Haskell.TH.Substitution ( Free(..), Subst(..), Sub(..) )
import Text.Themplates                  ( mapRight  )
import qualified Data.Map as M
import qualified Language.Haskell.Exts as Exts
import qualified Language.Haskell.Meta as Exts
import qualified Language.Haskell.TH.Lift as TH

type ErrorQ = EitherT String Q

nameOf :: Named a => a -> Name
nameOf = name_of

errorLeft :: Either String c -> c
errorLeft = either error id

errorQ :: String -> ErrorQ a
errorQ = hoistEither . Left

reifyCatch :: Name -> ErrorQ Info
reifyCatch n = do
  result <- lift $ recover (return Nothing) (Just <$> reify n)
  case result of
    Just v -> return v
    Nothing -> errorQ $ "Couldn't reify " ++ pprint n ++ "."

-- | Constructs a clause with a normal body and no where declarations.
normalClause :: [PatQ] -> ExpQ -> ClauseQ
normalClause ps e = clause ps (normalB e) []

normalMatch :: PatQ -> ExpQ -> MatchQ
normalMatch p e = match p (normalB e) []

toUInfix :: Exp -> Exp
toUInfix (InfixE (Just l) o (Just r)) = UInfixE l o r
toUInfix e = e

allUInfix :: Data a => a -> a
allUInfix = everywhere (id `extT` toUInfix)


qualifyName :: Name -> Q (Maybe Name)
qualifyName n = (name_of <$>)
            <$> recover (return Nothing) (Just <$> reify n)

qualifyNames :: (Free c, Subst c) => (Name -> Bool) -> c -> Q c
qualifyNames f x = do
  let vars = rights $ free x
  infos <- mapM qualifyName vars
  let rewrites = M.fromList . filter (f . fst)
               . catMaybes
               $ zipWith (\n -> ((n,) <$>)) vars infos
  return $ subst (Sub rewrites M.empty M.empty) x

dequalifyNames :: Data a => a -> a
dequalifyNames = everywhere (id `extT` (mkName . nameBase))

-- TODO: a more sophisticated set of types for representing and combining quasi-quoters.

-- | Expression QuasiQuoter
equasi :: (String -> ExpQ) -> QuasiQuoter
equasi f = QuasiQuoter f undefined undefined undefined

-- | Expression transformer QuasiQuoter
equasi' :: (Exp -> ExpQ) -> QuasiQuoter
equasi' f = equasi $ f . fromLeft . parseExp

-- | Pattern QuasiQuoter
pquasi :: (String -> PatQ) -> QuasiQuoter
pquasi f = QuasiQuoter undefined f undefined undefined

-- | Pattern transformer QuasiQuoter
pquasi' :: (Pat -> PatQ) -> QuasiQuoter
pquasi' f = pquasi $ f . fromLeft . parsePat

-- | Type QuasiQuoter
tquasi :: (String -> TypeQ) -> QuasiQuoter
tquasi f = QuasiQuoter undefined undefined f undefined

-- | Type transformer QuasiQuoter
tquasi' :: (Type -> TypeQ) -> QuasiQuoter
tquasi' f = tquasi $ f . fromLeft . parseType

-- | Declaration QuasiQuoter
dquasi :: (String -> DecsQ) -> QuasiQuoter
dquasi f = QuasiQuoter undefined undefined undefined f

-- | Declaration transformer QuasiQuoter
dquasi' :: ([Dec] -> DecsQ) -> QuasiQuoter
dquasi' f = dquasi $ f . fromLeft . parseDecs

debugInfoQ :: QuasiQuoter
debugInfoQ = equasi $ \xs -> [e| show $(TH.lift =<< reify (mkName xs)) |]

debug :: Show x => x -> x
debug x = trace (show x) x

fromLeft :: Either String a -> a
fromLeft = either error id

parseExp  :: String -> Either String Exp
parseExp  = mapRight Exts.toExp  . Exts.parseResultToEither
          . Exts.parseExpWithMode  parseMode
parsePat  :: String -> Either String Pat
parsePat  = mapRight Exts.toPat  . Exts.parseResultToEither
          . Exts.parsePatWithMode  parseMode
parseType :: String -> Either String Type
parseType = mapRight Exts.toType . Exts.parseResultToEither
          . Exts.parseTypeWithMode parseMode
parseDecs :: String -> Either String [Dec]
parseDecs = mapRight (\(Exts.Module _ _ _ _ _ _ x) -> Exts.toDecs x)
          . Exts.parseResultToEither
          . Exts.parseModuleWithMode parseMode
          . ("module Dummy where\n" ++)

-- TODO: If you wanted to make this do weird things / error, you could. Ohwell!
parseCxt :: String -> Either String Cxt
parseCxt s = process =<< parseDecs decs
 where
  decs = "instance " ++ s ++ " => I where"
  process [InstanceD cxt _ _] = Right cxt
  process _ = Left $ "Something strange happened while using the following to parse type contexts:\n" ++ decs

-- | Parse mode with all extensions and no fixities.
parseMode :: Exts.ParseMode
parseMode = Exts.ParseMode
  { Exts.parseFilename = ""
  , Exts.extensions = Exts.glasgowExts ++ [Exts.TupleSections, Exts.BangPatterns, Exts.ViewPatterns]
  , Exts.ignoreLinePragmas = False
  , Exts.ignoreLanguagePragmas = False
  , Exts.fixities = Nothing
  }
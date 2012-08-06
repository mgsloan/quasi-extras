{-# LANGUAGE FlexibleContexts, TupleSections, TemplateHaskell, QuasiQuotes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Quasi.Lenq
-- Copyright   :  (c) 2012 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This module provides quasi-quoters for conveniently constructing lenses
-- and bijections.  These are expressed by writing a getter, using a
-- restricted subset of Haskell, such that deriving a setter is possible.
--
-----------------------------------------------------------------------------
module Language.Quasi.Lenq (
-- * Configurable Bijection QuasiQuoter
    BijqConf(..), bijqQuoter, mkBijqConf, bijqExp, tupleBijExp

-- * Configurable Lens QuasiQuoter
  , LenqConf(..), lenqQuoter, mkLenqConf, lenqExp

-- * Miscellaneous Utilities
  , isoBwFromLens, patToExp, expToPat
  ) where

import Control.Applicative              ( (<$>), liftA )
import Data.List                        ( find, isPrefixOf )
import Data.Maybe                       ( fromJust )
import Data.Generics.Aliases            ( extT, extM )
import Data.Generics.Schemes            ( everywhere, everywhereM )
import Language.Haskell.Meta.Parse      ( parseExp, parseDecs )
import Language.Haskell.TH
import Language.Haskell.TH.Lib          ( unboxedTupE, unboxedTupP )
import Language.Haskell.TH.Build
import Language.Haskell.TH.Quote        ( QuasiQuoter(..) )
import Language.Quasi.Ast.TH            ( ec, d' )
import Language.Quasi.Internal.Conversion
import Language.Quasi.Internal.Desugar  ( dsParens )
import Text.Themplates                  ( generateNames )

--TODO: Think about deconstructor strictness - if every field of a subpattern
-- is matched, should the whole subpattern be Wild?

--TODO: makeLenses :: [Name] -> Q [Dec]
--TODO: makeLens :: Name -> Q [Dec]
--TODO: makePatLenses :: QuasiQuoter using pat syntx

--TODO: partiality

-- | Stores the expressions that should be used for the bijection constructor,
--   forward mapping, and backwards mapping.
--
--   When turned into Haskell, these should have the types
--   @ ((a -> b) -> (b -> a) -> BijType a b) @,
--   @ (BijType a b -> a -> b) @, and
--   @ (BijType a b -> b -> a) @ respectively.
data BijqConf = BijqConf { bijqConstr, bijFw, bijBw :: ExpQ }

-- | Stores the expressions that should be used for the lens constructor,
--   getter, and setter.
--
--   When turned into Haskell, these should have the types
--   @ ((a -> b) -> (b -> a -> a) -> LensType a b) @,
--   @ (LensType a b -> a -> b) @, and
--   @ (LensType a b -> b -> a -> a) @ respectively.
data LenqConf = LenqConf { lensConstr, lensGet, lensSet :: ExpQ }

-- | Convenience function to build a bijection quasi-quoter configuration from
--   stuff that's convertible to expressions.  See "Language.Haskell.TH.Build".
mkBijqConf :: (Convertible a ExpQ, Convertible b ExpQ, Convertible c ExpQ)
           => a -> b -> c -> BijqConf
mkBijqConf c f b = BijqConf (convert c) (convert f) (convert b)

-- | Convenience function to build a bijection quasi-quoter configuration from
--   stuff that's convertible to expressions.  See "Language.Haskell.TH.Build".
mkLenqConf :: (Convertible a ExpQ, Convertible b ExpQ, Convertible c ExpQ)
           => a -> b -> c -> LenqConf
mkLenqConf c g s = LenqConf (convert c) (convert g) (convert s)

-- | Throws the @Left@ as an error, otherwise yields the @Right@ value.
fromError :: (Either String a) -> a
fromError = either error id

-- | Creates a bijection quasi-quoter from a configuration.
bijqQuoter :: BijqConf -> QuasiQuoter
bijqQuoter conf = QuasiQuoter
  (                    bijqExp conf  . fromError . parseExp)
  undefined
  undefined
  ((processAsLambdas $ bijqExp conf) . fromError . parseDecs)
               
-- | Creates a lens quasi-quoter from a configuration.
lenqQuoter :: LenqConf -> QuasiQuoter
lenqQuoter conf = QuasiQuoter
  (                    lenqExp conf  . fromError . parseExp)
  undefined
  undefined
  ((processAsLambdas $ lenqExp conf) . fromError . parseDecs)

-- | Used for making lens setter into a backwards map.  This is used for
--   data-accessor and data-lens bijections.  This probably shouldn't exist,
--   as as @undefined@ is passed as the value for the "old state" of the
--   structure.
isoBwFromLens :: String -> Exp
isoBwFromLens = [ec| (`{{}}` undefined) |]

-- | Given a function that takes a lambda and yields an expression, applies
--   this function to every clause of every function declaration, by
--   translating the clause to a lambda.
processAsLambdas :: (Exp -> ExpQ) -> [Dec] -> DecsQ
processAsLambdas func = sequence . map doDec
 where
  --TODO: multiple clauses
  doDec [d'| {{f}} {{<... pn> ps}} = e |]
      = [d'| {{f}} {{<... pn> ps}} = {{ func $ LamE ps e }} |]
   where
  doDec _ = error "Where declarations and guards not supported by lenq!"

-- TODO: check bij well-formedness.

-- | Given a configuration and lambda expression, yields the corresponding
--   bijection (if possible - must be well-formed - TODO check more properties).
bijqExp :: BijqConf -> Exp -> Exp
bijqExp conf expr = case dsParens expr of
  [e'|     \ {{<... pn>      ps }} -> e |]
   -> [e'| \ {{<... pn> init ps }}
            -> {{ bijqConstr conf }}
               (\ {{ last ps    }} -> e                        )
               (\ {{ expToPat e }} -> {{ patToExp $ init ps }} )
         |]
  _ -> error "Bijq expressions must be lambdas."


-- TODO: check lens well-formedness / partiality?

-- | Given a configuration and lambda expression, yields the corresponding lens
--   (if possible - must be well-formed - TODO check more properties).
lenqExp :: [String] -> LenqConf -> Exp -> Exp
lenqExp names conf expr = case dsParens expr of
  [e'|      \ {{<... pn> ps}} -> e |]
    -> [e'| \ {{<... pn> init ps }}
             -> {{ lensConstr conf }}
                (\ {{ last ps }} -> e )
                (\ {{ expToPat e }} {{ decon }} -> {{ patToExp recon }} )
          |]
  _ -> error "Lenq expressions must be lambdas."
 where
  recon = everywhereM (return `extM` replaceWild)

{-
  reconPat

  expr = do
-- Replaces wildcards with variables, to preserve non-overwritten variables
    recon    <- everywhereM (return `extM` replaceWild) decon

-- Has wildcards for all overwritten variables
    let reconPat = everywhere (id `extT` wildWritten) recon

-- Name prefix of the non-changing variables in the structure
  varName = fromJust . find ((`notElem` namesBoundInPat decon) . mkName)
          $ iterate ('\'':) "v"

-- Replace wildcard with new variable.
  replaceWild WildP = VarP <$> newName varName
  replaceWild p     = return p

-- Replace variables that are set by wilds.
  wildWritten v@(VarP n)
    | not (varName `isPrefixOf` show n) = WildP
    | otherwise                         = v
  wildWritten v                         = v
 -}


tupleBijExp :: [String] -> BijqConf -> Name -> Exp
tupleBijExp conf n = mkBij =<< reify n
 where
  mkBij (TyConI (NewtypeD _ _ _  c  _)) = mkConBij c
  mkBij (TyConI (DataD    _ _ _ [c] _)) = mkConBij c
  mkBij x = error $ show x
         ++ " is not a Newtype or single-field single-constructor datatype."

  mkConBij c = do
    pat <- conToPat (const $ varP =<< newName "v") c
    bijqExp conf . LamE [pat] . TupE . map VarE $ namesBoundInPat pat
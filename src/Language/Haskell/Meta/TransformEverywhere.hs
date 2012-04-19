{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Meta.TransformEverywhere
-- Copyright   :  (c) 2012 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This module provides Template Haskell utilities, functions for using SYB
-- to create quasi-quoters.  Also included is an example usage of this function
-- to overload function application.
--
-----------------------------------------------------------------------------
module Language.Haskell.Meta.TransformEverywhere where

import Data.Data                 ( Data )
import Data.Generics.Aliases     ( extM )
import Data.Generics.Schemes     ( everywhereM )
import Language.Haskell.TH
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )
import Language.Haskell.Meta     ( parseExp, parsePat, parseType, parseDecs )

-- | Throws the @Left@ as an error, otherwise yields the @Right@ value.
fromError :: (Either String a) -> a
fromError = either error id

-- | Makes quasi-quotation into an AST transformer.
transformer :: (Exp   ->  ExpQ)
            -> (Pat   ->  PatQ)
            -> (Type  -> TypeQ)
            -> ([Dec] -> DecsQ)
            -> QuasiQuoter
transformer qe qp qt qd = QuasiQuoter
  (qe . fromError . parseExp)
  (qp . fromError . parsePat)
  (qt . fromError . parseType)
  (qd . fromError . parseDecs)

-- | Uses SYB to transform the AST everywhere.  Usually you want to have this
--   apply to a particular type
transformEverywhere :: (forall a. Data a => a -> Q a) -> QuasiQuoter
transformEverywhere f = transformer ef ef ef ef
 where
  ef :: Data a => a -> Q a
  ef = everywhereM f

-- | A quasiquoter that translates all instances of function application to
--   invocations of an "app" function.
-- 
-- > ( (1+) 5 ) becomes ( app (app (+) 1) 5 )
-- > ( (+1) 5 ) becomes ( app (app (app flip (+))))
overloadApp :: QuasiQuoter
overloadApp = overloadAppWith 
              (\l r -> appsE [varE $ mkName "app", return l, return r] )

-- | Takes every single instance of function application (and translatesinfix
--   operators appropriately), and replaces it using the passed function to
--   generate the new expression.
--
--   Things this doesn't handle:
--   * Unresolved infix operators - pretty much everything that's not a section.
--   * All of the de-sugarings of do-notation, comprehensions, enumeration
--     syntax, etc.
overloadAppWith :: (Exp -> Exp -> ExpQ) -> QuasiQuoter
overloadAppWith overload
  = transformEverywhere $ return `extM` handlePat `extM` handleExp
 where
  handleExp (AppE l r)                   = overload l r
  handleExp (InfixE (Just l) o Nothing)  = overload o l
  handleExp (InfixE Nothing  o (Just r)) = do
    f <- overload (VarE $ mkName "Prelude.flip") o
    overload f r
  handleExp (InfixE (Just l) o (Just r)) = overload o l >>= (`overload` r)
  --        (InfixE Nothing  o Nothing) is not a case - that's just "o"
  handleExp e = return e

  -- I count view patterns as function application.
  handlePat (ViewP e p) = viewP lam (return p)
   where lam = do n <- newName "x"
                  lamE [varP n] $ overload e (VarE n)
  handlePat e = return e
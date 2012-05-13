{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.TH.OverloadApp
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
module Language.Haskell.TH.OverloadApp where

import Data.Data                 ( Data )
import Data.Generics.Schemes     ( everywhereM )
import Language.Haskell.TH
import Language.Haskell.TH.Builders
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )
import Language.Haskell.Meta     ( toExp, toPat, toType, toDecs
                                 , parseResultToEither)

-- | Makes quasi-quotation into an AST transformer.
{-
transformer :: (Exp   ->  ExpQ)
            -> (Pat   ->  PatQ)
            -> (Type  -> TypeQ)
            -> ([Dec] -> DecsQ)
            -> QuasiQuoter
transformer qe qp qt qd = QuasiQuoter
  (qe . parseExp)
  (qp . parsePat)
  (qt . parseType)
  (qd . parseDecs)

-- | Uses SYB to transform the AST everywhere.  Usually you want to have this
--   apply to a particular type
transformEverywhere :: (forall a. Data a => a -> Q a) -> QuasiQuoter
transformEverywhere f = transformer ef ef ef ef
 where
  ef :: Data a => a -> Q a
  ef = everywhereM f
-}

-- | Translates 

{-
-- | A quasiquoter that translates all instances of function application to
--   invocations of an "app" function.
-- 
-- > ( (1+) 5 ) becomes ( app (app (+) 1) 5 )
-- > ( (+1) 5 ) becomes ( app (app (app flip (+))))
overloadApp :: QuasiQuoter
overloadApp = overloadAppWith 
              (\l r -> appsE [varE $ mkName "app", return l, return r] )

-- | Takes every single instance of function application (and translates infix
--   operators appropriately), and replaces it using the passed function to
--   generate the new expression.
--
--   Things this doesn't handle:
--   * All of the de-sugarings of do-notation, comprehensions, enumeration
--     syntax, etc.
overloadAppWith :: (Exp -> Exp -> ExpQ) -> QuasiQuoter
overloadAppWith overload
  = transformer ()
 where
  transform = everywhereM (return `extM` handlePat `extM` handleExp)
  handleExp (AppE l r) = overload l r
  handleExp e = return e

  -- I count view patterns as function application.
  handlePat (ViewP e p) = viewP lam (return p)
   where lam = do n <- newName "x"
                  lamE [varP n] $ overload e (VarE n)
  handlePat e = return e
-}
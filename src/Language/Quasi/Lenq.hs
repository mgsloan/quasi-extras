{-# LANGUAGE FlexibleContexts, TupleSections, TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Quasi.Lenq
-- Copyright   :  (c) 2012 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This module provides TH splices for conveniently constructing lenses
-- and bijections.  These are expressed by writing a getter, using a
-- restricted subset of Haskell, such that deriving a setter is possible.
--
-----------------------------------------------------------------------------
module Language.Quasi.Lenq (
-- * Lens
    lenqExp
  ) where

import Control.Lens

import Data.Generics ( extT, extM, everywhere, everywhereM, listify )
import Data.List     ( isPrefixOf, find )
import Data.Maybe    ( fromJust )
import Debug.Trace   ( trace )

import Language.Haskell.TH
import Language.Quasi.Internal.Conversion ( patToExp, expToPat' )

lenqExp :: ExpQ -> ExpQ
lenqExp src_exp_q = do
  src_exp <- src_exp_q
  case de_paren $ trace (pprint src_exp) src_exp of
    (LamE ps e) -> build_expr ps e
    _           -> error "lenq expressions must be lambdas."

 where
  de_paren (ParensE e) = de_paren e
  de_paren e = e

  build_expr ps e = do
    -- Pattern in getter
    let get_pat = last ps
  
    -- TODO: use variants that don't match on strings
    -- Replaces wildcards with variables, to preserve non-overwritten variables
    set_expr <- everywhereM (return `extM` replace_wild) get_pat

    -- Use wildcards for all overwritten variables
    let set_pat = everywhere (id `extT` wild_overwrite) set_expr

    lens_call <- [e| lens |]
    return
    -- Gets arguments
      . LamE (init ps)
    -- Lens partially applied to getter
      . AppE (AppE lens_call $ LamE [get_pat] e)
    -- Lambda for setter
      $ LamE [set_pat, expToPat' e] (patToExp set_expr)
   where
    all_names = map show $ listify (const True :: Name -> Bool) (ps, e)

    var_name = fromJust . find (\n -> not $ any (n `isPrefixOf`) all_names)
             $ map (\n -> 'x' : replicate n ''') [0..]
  
    -- Replace wildcard with new variable.
    replace_wild WildP = varP =<< newName var_name
    replace_wild p     = return p
  
    -- Replace variables that are set by wilds.
    wild_overwrite v@(VarP n)
      | not (var_name `isPrefixOf` show n) = WildP
      | otherwise    = v
    wild_overwrite v = v


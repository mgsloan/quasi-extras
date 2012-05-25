{-# LANGUAGE TemplateHaskell #-}

-- utility functions for debugging TH in GHCI
module Language.Haskell.TH.Debug where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift

debugQ :: Lift a => (String -> Q a) -> QuasiQuoter
debugQ f = QuasiQuoter helper undefined undefined undefined
 where
  helper s = do
   r <- f s
   return $ AppE (VarE $ mkName "show") r

$(deriveLiftMany
  [ ''Info
  , ''Fixity
  , ''Type
  , ''Dec
  , ''FixityDirection
  , ''Kind
  , ''FamFlavour
  , ''Pragma
  , ''Foreign
  , ''FunDep
  , ''Con
  , ''TyVarBndr
  , ''Pred
  , ''Body
  , ''Pat
  , ''Clause
  , ''InlineSpec
  , ''Safety
  , ''Callconv
  , ''Strict
  , ''Guard
  , ''Exp
  , ''Lit
  , ''Range
  , ''Stmt
  , ''Match
  ] )

debugInfoQ :: QuasiQuoter
debugInfoQ = debugQ (liftM lift . reify . mkName)

debugShowQ = debugQ ()

debugPPQ :: QuasiQuoter
debugInfoQ = debugQ ()
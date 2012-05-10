-- utility functions for debugging TH in GHCI

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift

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

reifyQ = QuasiQuoter helper undefined undefined undefined
 where
  helper s = do
   l <- lift =<< reify (mkName s)
   return $ AppE (VarE $ mkName "show") l

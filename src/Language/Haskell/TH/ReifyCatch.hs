module Language.Haskell.TH.ReifyCatch where

{-  ( ErrorQ, errorQ, normalClause, normalMatch
  , reifyCatch, reifyError
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.ReifyCatch.Internal


--reifyTyConI :: Name -> ErrorQ Dec
--reifyTyConI n = 

reifyDataD :: Name -> ErrorQ Dec
reifyDataD n = do
  i <- reifyCatch n
  case i of
    (TyConI d@(DataD _ _ _ _)) -> d
    _ -> reifyError "datatype constructor" n i
-}
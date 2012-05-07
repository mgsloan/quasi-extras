{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Language.Haskell.TH.ReifyCatch.Internal where

import qualified Data.Map as M
import qualified Language.Haskell.TH.Lift as TH

import Control.Applicative        ( (<$>) )
import Control.Monad              ( replicateM )
import Control.Monad.Trans.Class  ( lift )
import Control.Monad.Trans.Either ( EitherT(..), hoistEither )
import Data.Maybe                 ( fromMaybe )
import Language.Haskell.TH

type ErrorQ = EitherT String Q

errorQ :: String -> ErrorQ a
errorQ = hoistEither . Left

eterror :: ErrorQ a -> Q a
eterror x = either error return =<< runEitherT x

-- | Constructs a clause with a normal body and no where declarations.
normalClause :: [PatQ] -> ExpQ -> ClauseQ
normalClause ps e = clause ps (normalB e) []

normalMatch :: PatQ -> ExpQ -> MatchQ
normalMatch p e = match p (normalB e) []

reifyCatch :: Name -> ErrorQ Info
reifyCatch n = do
  result <- lift $ recover (return Nothing) (Just <$> reify n)
  case result of
    Just v -> return v
    Nothing -> errorQ $ "Couldn't reify " ++ pprint n ++ "."

reifyError :: String -> Name -> Info -> ErrorQ a
reifyError s n i = errorQ $ "Could not reify " ++ s ++ " for "
                ++ "'" ++ pprint n ++ "' - instead got:\n"
                ++ pprint i 

generateReifies :: [(String, String)] -> DecsQ
generateReifies descs = do
  (TyConI (DataD _ _ _ cs _)) <- reify $ mkName "Language.Haskell.TH.Info"
  mapM (\(NormalC n fs) -> mkDec n $ length fs) cs
 where
  descMap = M.fromList descs
  mkDec name l
    = funD (mkName $ "reify" ++ nameB) [clause [varP n] (normalB expr) []]
   where
    -- TODO: create better AST quasiquotation.
    n = mkName "n"
    expr = [| do { i <- reifyCatch $(varE n); $(cond) } |]
    i = mkName "i"
    cond = do
      names <- replicateM l (newName "v")
      caseE (varE i) 
        [ normalMatch (conP name $ map varP names) (tupE $ map varE names)
        , normalMatch wildP
            $ appsE
            [ [| reifyError |]
            , TH.lift . fromMaybe "correctly" $ M.lookup nameB descMap
            , varE n, varE i
            ]
        ]

    nameB = nameBase name

    pat = conP n $ replicate l wildP
    {-
case i of
                $(match)
                _ -> reifyError $(desc) $(varE ) i
-}
collectArrows :: [Type] -> Type -> [Type]
collectArrows xs (AppT (AppT ArrowT x) y) = collectArrows (x : xs) y
collectArrows xs t = t : xs
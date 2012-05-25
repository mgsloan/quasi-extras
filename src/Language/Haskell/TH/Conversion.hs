{-# LANGUAGE PatternGuards, TupleSections #-}
module Language.Haskell.TH.Conversion
  ( patToExp, expToPat, expToPat', expToPatMap, conToPat ) where

import Control.Applicative         ( liftA )
import qualified Data.Map  as M
import Language.Haskell.TH
-- TODO: remove
import Language.Haskell.TH.Build
import Language.Haskell.TH.Lib     ( unboxedTupE, unboxedTupP )

-- | Converts a pattern to an expression.
patToExp :: Pat -> Q Exp
patToExp (LitP l) = litE l
patToExp (VarP n) = varE n

patToExp (TupP        ps) =        tupE $ map patToExp ps
patToExp (UnboxedTupP ps) = unboxedTupE $ map patToExp ps
patToExp (ListP       ps) =       listE $ map patToExp ps

patToExp (ConP n ps) = appsE $ conE n : map patToExp ps
patToExp (RecP n fs) = recConE n $ map ( secondM patToExp ) fs
 where
  secondM f (x, y) = liftA (\y' -> (x , y')) $ f y

patToExp (InfixP  l n r) =  infixE (Just $ patToExp l) (varE n) (Just $ patToExp r)
patToExp (UInfixP l n r) = uInfixE (       patToExp l) (varE n) (       patToExp r)

patToExp (SigP    p t) =  sigE (patToExp p) (return t)
patToExp (ParensP p) = parensE $ patToExp p
patToExp (TildeP  p) =           patToExp p
patToExp (BangP   p) =           patToExp p

patToExp (AsP   _ _) = error   "AsP has no expression equivalent."
patToExp (ViewP _ _) = error "ViewP has no expression equivalent."
patToExp (WildP    ) = error "WildP has no expression equivalent."

expToPat' :: Exp -> PatQ
expToPat' = expToPat (const $ error "Cannot convert function application to pattern.")

expToPatMap :: (M.Map String ([Exp] -> PatQ)) -> Exp -> PatQ
expToPatMap m = expToPat fallback
 where
  fallback (VarE fn:xs)
    | Just func <- M.lookup (pprint fn) m = func xs
    | otherwise = error $ "Could not convert function to pattern: " ++ pprint fn
  fallback e = error $ "Could not convert: " ++ pprint e

-- | Converts an expression to a pattern.
expToPat :: ([Exp] -> PatQ) -> Exp -> PatQ
expToPat _ (LitE l) = litP l
expToPat _ (VarE n) = varP n

expToPat f (TupE          ps) =        tupP $ map (expToPat f) ps
expToPat f (UnboxedTupE   ps) = unboxedTupP $ map (expToPat f) ps
expToPat f (ListE         ps) =       listP $ map (expToPat f) ps

expToPat f (SigE    p t) = sigP (expToPat f p) (return t)
expToPat f (ParensE p)   = parensP' $ expToPat f p

expToPat f ( InfixE (Just l) (ConE n) (Just r)) =  infixP' (expToPat f l) n (expToPat f r)
expToPat f (UInfixE       l  (ConE n)       r ) = uInfixP' (expToPat f l) n (expToPat f r)

expToPat _ (ConE n) = conP n []

expToPat f e@(AppE _ _)
  = case collect e [] of
      (ConE n:xs) -> conP' n $ map (expToPat f) xs
      xs -> f xs -- error $ "Cannot convert function application to pattern, in " + pprint e
 where
  collect (AppE l r) xs = collect l (r:xs)
  collect x          xs = x:xs

expToPat _ e = error $ " has no pattern equivalent:\n" ++ show e 

-- TODO expToPat   (RecE        n fs) = recConP' n $ map (second expToPat) fs

-- Converts a constructor declaration to a pattern.
conToPat :: (Either Type (Name, Type) -> PatQ) -> Con -> PatQ
conToPat f (NormalC   n xs) = conP n $ map (f . Left . snd) xs
conToPat f (InfixC tl n tr) = conP n $ map (f . Left . snd) [tl, tr]
conToPat f (RecC      n fs) = recP n $ map applyToField fs
 where applyToField (fn, _, t) = liftA (fn,) . f $ Right (fn, t)
conToPat f (ForallC  _ _ c) = conToPat f c





{-
typToPat :: Type -> PatQ
typToPat (ConT n) = conP n
typToPat a@(AppT _ _) = collectAppsT helper [] a
 where
  -- TODO: consider checking arity?
  helper (TupleT        _) xs = TupP        xs
  helper (UnboxedTupleT _) xs = UnboxedTupP xs
typToPat t = error "Cannot convert Type to Pattern." 

collectAppsT :: (Type -> [Type] -> a) -> a
collectAppsT f xs (AppT l r) = collectAppsT f (r:xs) l
collectAppsT f xs t             = f (typToPat t) (map typToPat xs)

-}
{-# LANGUAGE PatternGuards, TupleSections #-}
module Language.Quasi.Internal.Conversion
  ( patToExp, expToPat, expToPat', expToPatMap, conToPat ) where

import Control.Arrow             ( second )
import qualified Data.Map as M
import Language.Haskell.TH
import Language.Haskell.TH.Lib   ( unboxedTupE, unboxedTupP )

-- | Converts a pattern to an expression.
patToExp :: Pat -> Exp
patToExp (LitP l) = LitE l
patToExp (VarP n) = VarE n

patToExp (TupP        ps) =        TupE $ map patToExp ps
patToExp (UnboxedTupP ps) = UnboxedTupE $ map patToExp ps
patToExp (ListP       ps) =       ListE $ map patToExp ps

patToExp (ConP n ps) = foldl AppE (ConE n) $ map patToExp ps
patToExp (RecP n fs) = RecConE n $ map ( second patToExp ) fs

patToExp (InfixP  l n r) =  InfixE (Just $ patToExp l) (VarE n) (Just $ patToExp r)
patToExp (UInfixP l n r) = UInfixE (       patToExp l) (VarE n) (       patToExp r)

patToExp (SigP  p t) = SigE (patToExp p) t
patToExp (ParensP p) = ParensE $ patToExp p
patToExp (TildeP  p) =           patToExp p
patToExp (BangP   p) =           patToExp p

patToExp (AsP   _ _) = error   "AsP has no expression equivalent."
patToExp (ViewP _ _) = error "ViewP has no expression equivalent."
patToExp (WildP    ) = error "WildP has no expression equivalent."

expToPat' :: Exp -> Pat
expToPat' = expToPat (const $ error "Cannot convert function application to pattern.")

expToPatMap :: (M.Map String ([Exp] -> Pat)) -> Exp -> Pat
expToPatMap m = expToPat fallback
 where
  fallback (VarE fn:xs)
    | Just func <- M.lookup (pprint fn) m = func xs
    | otherwise = error $ "Could not convert function to pattern: " ++ pprint fn
  fallback e = error $ "Could not convert: " ++ pprint e

-- | Converts an expression to a pattern.
expToPat :: ([Exp] -> Pat) -> Exp -> Pat
expToPat _ (LitE l) = LitP l
expToPat _ (VarE n) = VarP n

expToPat f (TupE          ps) =        TupP $ map (expToPat f) ps
expToPat f (UnboxedTupE   ps) = UnboxedTupP $ map (expToPat f) ps
expToPat f (ListE         ps) =       ListP $ map (expToPat f) ps

expToPat f (SigE  p t) = SigP (expToPat f p) t
expToPat f (ParensE p) = ParensP $ expToPat f p

expToPat f ( InfixE (Just l) (ConE n) (Just r)) =  InfixP (expToPat f l) n (expToPat f r)
expToPat f (UInfixE       l  (ConE n)       r ) = UInfixP (expToPat f l) n (expToPat f r)

expToPat _ (ConE n) = ConP n []

expToPat f e@(AppE _ _)
  = case collect e [] of
      (ConE n:xs) -> ConP n $ map (expToPat f) xs
      xs -> f xs -- error $ "Cannot convert function application to pattern, in " + pprint e
 where
  collect (AppE l r) xs = collect l (r:xs)
  collect x          xs = x:xs

expToPat _ e = error $ " has no pattern equivalent:\n" ++ show e 

-- TODO expToPat   (RecE        n fs) = recConP' n $ map (second expToPat) fs

-- Converts a constructor declaration to a pattern.
conToPat :: (Either Type (Name, Type) -> Pat) -> Con -> Pat
conToPat f (NormalC   n xs) = ConP n $ map (f . Left . snd) xs
conToPat f (InfixC tl n tr) = ConP n $ map (f . Left . snd) [tl, tr]
conToPat f (RecC      n fs) = RecP n $ map applyToField fs
 where applyToField (fn, _, t) = (fn,) . f $ Right (fn, t)
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
{-# LANGUAGE TypeFamilies, FlexibleInstances, TypeSynonymInstances #-}
module Language.Haskell.TH.Convenience where

import Control.Applicative ((<$>), (<*>))
import Data.Char (isUpper)

import Language.Haskell.TH

class ToExpQ      a where toExpQ      :: a -> ExpQ
class ToPatQ      a where toPatQ      :: a -> PatQ
class ToTypeQ     a where toTypeQ     :: a -> TypeQ
class ToDecQ      a where toDecQ      :: a -> DecQ
class ToBodyQ     a where toBodyQ     :: a -> BodyQ
class ToStmtQ     a where toStmtQ     :: a -> StmtQ
class ToMatchQ    a where toMatchQ    :: a -> MatchQ
class ToGuardQ    a where toGuardQ    :: a -> GuardQ
class ToTyVarBndr a where toTyVarBndr :: a -> TyVarBndr

instance ToExpQ   ExpQ   where toExpQ   = id
instance ToPatQ   PatQ   where toPatQ   = id
instance ToTypeQ  TypeQ  where toTypeQ  = id
instance ToDecQ   DecQ   where toDecQ   = id
instance ToBodyQ  BodyQ  where toBodyQ  = id
instance ToStmtQ  StmtQ  where toStmtQ  = id
instance ToMatchQ MatchQ where toMatchQ = id
instance ToGuardQ GuardQ where toGuardQ = id

instance ToExpQ   Exp   where toExpQ   = return
instance ToPatQ   Pat   where toPatQ   = return
instance ToTypeQ  Type  where toTypeQ  = return
instance ToDecQ   Dec   where toDecQ   = return
instance ToBodyQ  Body  where toBodyQ  = return
instance ToStmtQ  Stmt  where toStmtQ  = return
instance ToMatchQ Match where toMatchQ = return
instance ToGuardQ Guard where toGuardQ = return

ifCap :: (Name -> a) -> (Name -> a) -> Name -> a
ifCap f g n = if isUpper . head $ nameBase n then f n else g n

instance ToExpQ      Name where toExpQ      = ifCap conE varE
instance ToPatQ      Name where toPatQ      = ifCap (`conP` []) varP
instance ToTypeQ     Name where toTypeQ     = ifCap conT varT
instance ToBodyQ     Name where toBodyQ     = toBodyQ . toExpQ
instance ToStmtQ     Name where toStmtQ     = toStmtQ . toExpQ
instance ToTyVarBndr Name where toTyVarBndr = PlainTV

instance ToExpQ      String where toExpQ      = toExpQ      . mkName
instance ToPatQ      String where toPatQ      = toPatQ      . mkName
instance ToTypeQ     String where toTypeQ     = toTypeQ     . mkName
instance ToBodyQ     String where toBodyQ     = toBodyQ     . mkName 
instance ToStmtQ     String where toStmtQ     = toStmtQ     . mkName 
instance ToTyVarBndr String where toTyVarBndr = toTyVarBndr . mkName

instance ToBodyQ Exp              where toBodyQ = normalB . return
instance ToBodyQ ExpQ             where toBodyQ = normalB
--TODO: good idea?
instance ToBodyQ [  (Guard, Exp)] where toBodyQ = guardedB . map return
instance ToBodyQ [Q (Guard, Exp)] where toBodyQ = guardedB
instance ToBodyQ [(GuardQ, ExpQ)] where toBodyQ = toBodyQ . map (\(g, e) -> (,) <$> g <*> e)

instance ToGuardQ  Exp    where toGuardQ = normalG . return
instance ToGuardQ  ExpQ   where toGuardQ = normalG
instance ToGuardQ  Stmt   where toGuardQ = patG . (:[]) . return
instance ToGuardQ  StmtQ  where toGuardQ = patG . (:[])
instance ToGuardQ [Stmt ] where toGuardQ = patG . map return
instance ToGuardQ [StmtQ] where toGuardQ = patG

instance ToStmtQ Exp  where toStmtQ = noBindS . return
instance ToStmtQ ExpQ where toStmtQ = noBindS

--TODO: good ideas?
instance ToTyVarBndr (Name, Kind) where toTyVarBndr = uncurry KindedTV
-- instance ToPred (Name, [Type])
-- instance ToPred (Type, Type)

instance (ToPatQ p, ToBodyQ b, ToDecQ d) => ToMatchQ (p, b, [d]) where toMatchQ (p, b, ds) = match (toPatQ p) (toBodyQ b) (map toDecQ ds)
instance (ToPatQ p, ToBodyQ b)           => ToMatchQ (p, b     ) where toMatchQ (p, b    ) = match (toPatQ p) (toBodyQ b)              []
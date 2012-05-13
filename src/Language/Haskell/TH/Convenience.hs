{-# LANGUAGE TypeFamilies, FlexibleInstances, TypeSynonymInstances #-}
module Language.Haskell.TH.Convenience
  (   toExp,   toPat,   toType,   toDec,   toBody,   toStmt,   toMatch,   toGuard,   toTyVarBndr
  , fromExp, fromPat, fromType, fromDec, fromBody, fromStmt, fromMatch, fromGuard, fromTyVarBndr
  )
  where

import Control.Applicative       ((<$>), (<*>))
import Data.Char                 (isUpper)
import Language.Haskell.TH.Named ( name_of )

import Language.Haskell.TH

class ToExp      a where toExp      :: a -> Exp
class ToPat      a where toPat      :: a -> Pat
class ToType     a where toType     :: a -> Type
class ToDec      a where toDec      :: a -> Dec
class ToBody     a where toBody     :: a -> Body
class ToStmt     a where toStmt     :: a -> Stmt
class ToMatch    a where toMatch    :: a -> Match
class ToGuard    a where toGuard    :: a -> Guard
class ToTyVarBndr a where toTyVarBndr :: a -> TyVarBndr

instance ToExp   Exp   where toExp   = id
instance ToPat   Pat   where toPat   = id
instance ToType  Type  where toType  = id
instance ToDec   Dec   where toDec   = id
instance ToBody  Body  where toBody  = id
instance ToStmt  Stmt  where toStmt  = id
instance ToMatch Match where toMatch = id
instance ToGuard Guard where toGuard = id

ifCap :: (Name -> a) -> (Name -> a) -> Name -> a
ifCap f g n = if isUpper . head $ nameBase n then f n else g n

instance ToExp       Name where toExp       = ifCap ConE VarE
instance ToPat       Name where toPat       = ifCap (`ConP` []) VarP
instance ToType      Name where toType      = ifCap ConT VarT
instance ToBody      Name where toBody      = toBody . toExp
instance ToStmt      Name where toStmt      = toStmt . toExp
instance ToTyVarBndr Name where toTyVarBndr = PlainTV

instance ToExp       String where toExp       = toExp      . mkName
instance ToPat       String where toPat       = toPat      . mkName
instance ToType      String where toType      = toType     . mkName
instance ToBody      String where toBody      = toBody     . mkName 
instance ToStmt      String where toStmt      = toStmt     . mkName 
instance ToTyVarBndr String where toTyVarBndr = toTyVarBndr . mkName

instance ToBody Exp            where toBody = NormalB
--TODO: good idea?
instance ToBody [(Guard, Exp)] where toBody = GuardedB

instance ToGuard  Exp   where toGuard = NormalG
instance ToGuard  Stmt  where toGuard = PatG . (:[])
instance ToGuard [Stmt] where toGuard = PatG

instance ToStmt Exp where toStmt = NoBindS

--TODO: good ideas?
instance ToTyVarBndr (Name, Kind) where toTyVarBndr = uncurry KindedTV
-- instance ToPred (Name, [Type])
-- instance ToPred (Type, Type)

instance (ToPat p, ToBody b, ToDec d) => ToMatch (p, b, [d]) where toMatch (p, b, ds) = Match (toPat p) (toBody b) (map toDec ds)
instance (ToPat p, ToBody b)          => ToMatch (p, b     ) where toMatch (p, b    ) = Match (toPat p) (toBody b)              []


class FromExp       a where fromExp       :: Exp       -> a
class FromPat       a where fromPat       :: Pat       -> a
class FromType      a where fromType      :: Type      -> a
class FromDec       a where fromDec       :: Dec       -> a
class FromBody      a where fromBody      :: Body      -> a
class FromStmt      a where fromStmt      :: Stmt      -> a
class FromMatch     a where fromMatch     :: Match     -> a
class FromGuard     a where fromGuard     :: Guard     -> a
class FromTyVarBndr a where fromTyVarBndr :: TyVarBndr -> a

instance FromExp   Exp   where fromExp   = id
instance FromPat   Pat   where fromPat   = id
instance FromType  Type  where fromType  = id
instance FromDec   Dec   where fromDec   = id
instance FromBody  Body  where fromBody  = id
instance FromStmt  Stmt  where fromStmt  = id
instance FromMatch Match where fromMatch = id
instance FromGuard Guard where fromGuard = id

-- TODO

{-
instance FromExp       Name where fromExp       = name_of
instance FromPat       Name where fromPat       = name_of
instance FromType      Name where fromType      = name_of
instance FromBody      Name where fromBody      = name_of
instance FromStmt      Name where fromStmt      = name_of
instance FromTyVarBndr Name where fromTyVarBndr = name_of
-}

{-
instance FromExp       String where fromExp       = toExp      . mkName
instance FromPat       String where fromPat       = toPat      . mkName
instance FromType      String where fromType      = toType     . mkName
instance FromBody      String where fromBody      = toBody     . mkName 
instance FromStmt      String where fromStmt      = toStmt     . mkName 
instance FromTyVarBndr String where fromTyVarBndr = toTyVarBndr . mkName

instance FromBody Exp              where fromBody = normalB . return
instance FromBody Exp             where fromBody = normalB
--TODO: good idea?
instance FromBody [  (Guard, Exp)] where fromBody = guardedB . map return
instance FromBody [ (Guard, Exp)] where fromBody = guardedB
instance FromBody [(Guard, Expeval)] where fromBody = toBody . map (\(g, e) -> (,) <$> g <*> e)

instance FromGuard  Exp    where fromGuard = normalG . return
instance FromGuard  Exp   where fromGuard = normalG
instance FromGuard  Stmt   where fromGuard = patG . (:[]) . return
instance FromGuard  Stmt  where fromGuard = patG . (:[])
instance FromGuard [Stmt ] where fromGuard = patG . map return
instance FromGuard [Stmt] where fromGuard = patG

instance FromStmt Exp  where fromStmt = noBindS . return
instance FromStmt Exp where fromStmt = noBindS
-}

--TODO: good ideas?
--instance FromTyVarBndr (Name, Kind) where fromTyVarBndr = uncurry KindedTV
-- instance ToPred (Name, [Type])
-- instance ToPred (Type, Type)

--instance (FromPat p, FromBody b, FromDec d) => FromMatch (p, b, [d]) where toMatch (p, b, ds) = match (toPat p) (toBody b) (map toDec ds)
--instance (FromPat p, FromBody b)             => FromMatch (p, b     ) where toMatch (p, b    ) = match (toPat p) (toBody b)              []
{-# LANGUAGE QuasiQuotes, TemplateHaskell, ViewPatterns, DoAndIfThenElse #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.TH.Desugar
-- Copyright   :  (c) 2012 Michael Sloan
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-- This module provides Template Haskell utilities for desugaring expressions,
-- as in the Standard Report: <http://www.haskell.org/onlinereport/exps.html>
--
-----------------------------------------------------------------------------
module Language.Haskell.TH.Desugar where

import qualified Data.Map as M

import Control.Arrow              ( first )
import Control.Monad              ( liftM )
import Control.Monad.Trans.Class  ( lift )
import Control.Applicative        ( (<$>) )
import Data.Foldable              ( foldlM )
import Data.Generics              ( Data )
import Data.Generics.Aliases      ( extT, extM )
import Data.Generics.Schemes      ( everywhere, everywhereM )
import Data.List                  ( find, intersperse )
import Data.Maybe                 ( fromJust )
import Language.Haskell.TH
import Language.Haskell.TH.Ppr    ( pprString ) 
import Language.Haskell.TH.PprLib ( punctuate, comma, quotes )
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Builders
import Language.Haskell.TH.Convenience
import Language.Haskell.TH.ReifyCatch.Internal

{-

-- Observation:  In general, as things using TH approach language extensions / 
-- plugins / advanced DSLs, interoperating with GHC's functionality becomes more
-- and more valuable.  Currently this isn't at all possible.  For example, it
-- would be really cool to be able to process and generate GHC core.
--
-- In the mean-time, we need to implement all of the GHC stuff, like desugarings,
-- or parsing (haskell-src-exts / meta), over again, in TH-land.
--
-- One interesting way this could go is if the TH stuff could be developed to
-- such an extent that parts of the language could start moving into libraries.
-- These de-sugarings are much more compact than GHC's, yet other than issues
-- with error messages, ought to be functionally similar (identical semantics).


-- TODO: Clean up handling of errors.
-- TODO: Resugarings.

-- Utilities
eerror :: Either String a -> a
eerror = either error id

allUInfix :: Exp -> Exp
allUInfix = everywhere (id `extT` convert)
 where
  convert (InfixE (Just l) o (Just r)) = UInfixE l o r
  convert e = e

quoteCommaPpr :: Ppr a => [a] -> String
quoteCommaPpr xs = show . punctuate comma $ map (quotes . ppr) xs


-- | Constructs the specified infix operator, combining the @Exp@ arguments.
opE :: ExpQ -> String -> ExpQ -> ExpQ
opE l o r = infixE (Just l) (varE $ mkName o) (Just r)

-- | Unwraps @ForallC@s.
unforallCon :: Con -> Con
unforallCon (ForallC _ _ c) = c
unforallCon c = c

-- | Extracts @Left@ the value, calling @error@ on failure.
fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _ = error "fromLeft"

-- | Extracts @Right@ the value, calling @error@ on failure.
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "fromRight"


-- | Applies the de-sugarings that I think would be useful for doing
--   interesting code transformations.
{-
desugar :: forall a. Data a => a -> Q a
desugar = everywhereM (return `extM` helper)
 where
  helper (InfixE    l o r) =           dsInfix    l o r
  helper (DoE          xs) = eterror $ dsDo          xs
  helper (CompE        xs) = eterror $ dsDo          xs
  helper (RecConE    n fs) = eterror $ dsRecCon    n fs 
  helper (ListE        xs) =           dsList        xs
--  helper (SigE        e t) =           dsSig        e t
--  helper (LamE       ps e) =           dsLambda    ps e
  helper (ArithSeqE     r) =           dsArithSeq     r
  helper (CondE     c t e) =           dsIf       c t e
  helper                e  =           return         e
-}


-- | Desugars an infix operator.  This is close to the way that the Standard
--   Report de-sugars infix-operators:
-- 
--   (From Section 3.4: "Operator Applications")
--   The following identities hold:
-- @
--   e1 op e2 = (op) e1 e2
--   -e       = negate e
-- @
--
--   (From Section 3.5: "Sections")
--   The following identities hold:
-- @
--   (op e) = \x -> x op e
--   (e op) = \x -> e op x
-- @
--   where @op@ is a binary operator, @e@ is an expression, and @x@ is a
--   variable that does not occur free in @e@.
--
--   Negation is already represented using @App@E in TH.  This leaves us with
--   one last consideration: @-XPostfixOperators@.  This extension makes it such
--   that @ (e !) @ is equivalent (from the point of view of both type checking
--   and execution) to the expression @ ((!) e) @.
--dsInfix :: (Maybe Exp) -> Exp -> (Maybe Exp) -> ExpQ
dsInfix :: (Maybe Exp) -> Exp -> (Maybe Exp) -> ExpQ
dsInfix Nothing  o Nothing  = return o
dsInfix (Just l) o Nothing  = return [thExp| $(o) $(l) |]
dsInfix (Just l) o (Just r) = return [thExp| $(o) $(l) $(r) |]
dsInfix Nothing  o (Just r) = newName "x" >>= \x
                           -> return [thExp| \$(VarP x) -> $(VarE x) $(o) $(r) |]
{- --TODO
dsInfix [thExp|  $(l) `$(o)` $(r)  |] = return [thExp| $(o) $(l) $(r) |]
dsInfix [thExp| ($(l) `$(o)`     ) |] = return [thExp| $(o) $(l) |]
dsInfix [thExp| (     `$(o)` $(r)) |] = newName "x" >>= \x
                                     -> return [thExp| $(x) $(o) $(r) |]
dsInfix e = return e
-}

-- | Desugars the statements involved in a do-block, as described in the
--   Standard Report:
--
--   Do expressions satisfy these identities, which may be used as a
--   translation into the kernel, after eliminating empty stmts:
-- @
--   do {e}  = e
--   do {e;stmts}  = e >> do {stmts}
--   do {p <- e; stmts}  = let ok p = do {stmts}
--       ok _ = fail "..."
--     in e >>= ok
--   do {let decls; stmts} = let decls in do {stmts}
-- @
--   The ellipsis \"...\" stands for a compiler-generated error message, passed
--   to fail, preferably giving some indication of the location of the
--   pattern-match failure; the functions @>>@, @>>=@, and @fail@ are operations
--   in the class Monad, as defined in the Prelude; and @ok@ is a fresh
--   identifier.
--
--   The behaviour deviates slightly from the Standard Report, and instead
--   implements @-XRebindableSyntax@ - the in scope monad operators are used,
--   and must be in scope.
-- 
--   This implementation also handles the @-XParallelListComp@ extension, as
--   this code is also used for desugaring list comprehensions.
dsDo :: [Stmt] -> ErrorQ Exp
dsDo [] = errorQ "Empty 'do' block"

dsDo [NoBindS e] = return e

dsDo [e] = errorQ
  $ "The last statement in a 'do' block must be an expression " ++ pprint e

dsDo (x:xs) = process =<< dsDo xs
 where
  process rest = case x of
    (LetS ds)   -> return $ LetE ds rest
    (BindS p e) -> lift $ processBind p e rest
    (NoBindS e) -> lift $ opE (return e) ">>" (return rest)
    --(ParS xs)   -> 

  processBind p e rest = do
    ok <- newName "ok"
    letE [funD ok [ normalClause [return p] (return rest)
                  , normalClause [wildP] matchFail
                  ]
         ]
         (opE (return e) ">>=" (varE ok))
  matchFail = [| fail "Pattern match failure in do expression" |]


-- | De-sugars lists as described in the Standard Report:
--
--   The following identity holds:
-- @
--   [e1, ..., ek]   =   e1 : (e2 : ( ... (ek : [])))
-- @
--   where @:@ and @[]@ are constructors for lists, as defined in the Prelude
--   (see Section 6.1.3). The types of @e1@ through @e@k must all be the same
--   (call it @t@), and the type of the overall expression is @[t]@
--   (see Section 4.1.2).
dsList :: Exp -> Exp
dsList (ListE xs) = foldr (\l r -> [thExp| $(l) : $(r) |]) (ListE []) xs


-- | De-sugars arithmetic sequences as described in the Standard Report:
--
--   Arithmetic sequences satisfy these identities:
-- @
--   [ e1.. ]  = enumFrom e1
--   [ e1,e2.. ] = enumFromThen e1 e2
--   [ e1..e3 ]  = enumFromTo e1 e3
--   [ e1,e2..e3 ] = enumFromThenTo e1 e2 e3
-- @
--   where enumFrom, enumFromThen, enumFromTo, and enumFromThenTo are class
--   methods in the class Enum as defined in the Prelude (see Figure 6.1).
dsArithSeq (ArithSeqE r) = case r of
 (FromR       f    ) -> [thExp| enumFrom       $(f)           |]
 (FromThenR   f n  ) -> [thExp| enumFromThen   $(f) $(n)      |]
 (FromToR     f   t) -> [thExp| enumFromTo     $(f)      $(t) |]
 (FromThenToR f n t) -> [thExp| enumFromThenTo $(f) $(n) $(t) |]


--TODO: What is "ExplicitPArr", PArrSeq, etc?


-- | Desugars if statements as described in the Standard Report:
--
--   The following identity holds:
-- @
--   if e1 then e2 else e3 = case e1 of { True -> e2 ; False -> e3 }
-- @
--   where @True@ and @False@ are the two nullary constructors from the type
--   @Bool@, as defined in the Prelude. The type of @e1@ must be @Bool@; @e2@
--   and @e3@ must have the same type, which is also the type of the entire
--   conditional expression.
--
--   However, in order to be compatible with @-XRebindableSyntax@, it checks if
--   @ifThenElse@ is in scope.  If so, then the desugaring is as follows:
-- @
--   if e1 then e2 else e3 = ifThenElse e1 e2 e3
-- @
dsIf :: Exp -> Exp -> Exp -> ExpQ
dsIf c t e = recover (return fallback) (reify ifThenElse >> return overloaded)
 where
  ifThenElse = mkName "ifThenElse"
  overloaded = [thExp'| $(ifThenElse) $(c) $(t) $(e) |]
  fallback = [thExp'| case $(c) of { True -> $(t); False -> $(e) } |]


-- | Desugars record construction as described in the Standard Report:
--
--   In the binding @f = v@, the field @f@ labels @v@.
-- @
--   C { bs }  = C (pickC1 bs undefined) ...(pickCk bs undefined)
-- @
--   where @k@ is the arity of @C@.
--
--   The auxiliary function @pickCi bs d@ is defined as follows:
--   
--   If the @i@th component of a constructor @C@ has the field label @f@, and
--   if @f=v@ appears in the binding list @bs@, then @pickCi bs d@ is @v@.
--   Otherwise, @pickCi bs d@ is the default value @d@.
dsRecCon :: Name -> [FieldExp] -> ErrorQ Exp
dsRecCon cn fes = process =<< lookupConstructors cn
 where
  femap = M.fromList $ map (first nameBase) fes

  process fs = lift $ appsE (conE cn : map pickC fs)
   where
    pickC ((`M.lookup` femap) . nameBase -> Just e, _, _) = return e
    pickC _ = varE $ mkName "undefined"

  lookupConstructors n = do
    -- There's gotta be a better way to reify a constructor's field description.
    info <- reifyCatch n
    case info of
      (DataConI _ _ n' _)
        | n /= n' -> lookupConstructors n'
        | otherwise -> errorQ $ "Could not reify type constructor for "
                             ++ "'" ++ pprint n ++ "' - instead got:\n"
                             ++ pprint info
      (TyConI dd@(DataD _ _ _ cs _)) -> do
        case find recWereLookingFor $ map unforallCon cs of
          Just (RecC _ fs) -> return fs
          _ -> errorQ $ "Could not find record constructor '" ++ pprint n ++ "' in:\n"
                     ++ pprint dd
      _ -> errorQ $ "Could not desugar record type:\n"
                 ++ pprint info

  recWereLookingFor (RecC n' _) = nameBase cn == nameBase n'
  recWereLookingFor _ = False


-- | Desugars record field updates as described in the Standard Report:
--
--   Using the prior definition of pick,
-- @
--   e { bs }  = case e of
--           C1 v1 ... vk1 -> C1 (pickC11 bs v1) ... (pickC1k1 bs vk1)
--                ...
--           Cj v1 ... vkj -> Cj (pickCj1 bs v1) ... (pickCjkj bs vkj)
--           _ -> error "Update error"
-- @
--   where {C1,...,Cj} is the set of constructors containing all labels in bs,
--   and ki is the arity of Ci.
{- TODO
dsRecUpd :: Exp -> [FieldExp] -> ErrorQ Exp
dsRecUpd e fes = do
  conType <- getConType
  info <- reifyCatch conType

  case info of
    (TyConI (DataD _ _ _ cs _))
      -> buildResult =<< (concat <$> mapM (mkMatch . unforallCon) cs)
    _ -> errorQ $ "Type " ++ pprint conType ++ " does not reify to a DataType constructor. Instead\n"
               ++ pprint info
 where
  femap = M.fromList $ map (first nameBase) fes

  buildResult [] = errorQ $ "No constructor has all these fields: " ++ nameList
  buildResult matches
    = lift . caseE (return e)
    $ matches ++ [match wildP (normalB [| error "Update error" |]) []]

  mkMatch (RecC n fs) = do

    let results :: [(String, Maybe ExpQ)]
        results = map (\(nameBase -> n, _, _) -> (n, M.lookup n femap)) fs

    names <- mapM (\(n, m) -> maybe (newName n) (return . const under) m) results

    let patterns = map (ifUnder wildP varP) names
        vals = zipWith (\(_, e) -> ifUnder (return $ fromJust e) varE) results names

    return [match (conP n patterns) (normalB . appsE $ conE n : vals)]
   where
    under = mkName "_"
    ifUnder u f n = if n == under then u else f n
  mkMatch _ = return [] 

  nameList = quoteCommaPpr (map fst fes)

  getConType :: ErrorQ Name
  getConType = do
    ts <- mapM (reifyConType . fst) fes
    if all (uncurry (==)) . zip ts $ tail ts
    then return $ head ts
    else errorQ $ "The following constructors come from different types:\n"
               ++ nameList ++ " ( " ++ quoteCommaPpr ts ++ " respectively )"

  reifyConType :: Name -> ErrorQ Name
  reifyConType fn = do
    info <- reifyCatch fn
    case info of
      (VarI _ (collectArrows [] -> (ConT cn:_)) _ _) -> return cn
      _ -> errorQ $ "'" ++ pprint fn ++ "' doesn't reify as a field selector!"
-}


-- | Desugars record field selectors as described in the Standard Report:
--
--   A field label @f@ introduces a selector function defined as:
-- @
--   f x = case x of { C1 p11 ...p1k  ->  e1 ; ... ; Cn pn1 ...pnk  ->  en }
-- @
--   where @C1...Cn@ are all the constructors of the datatype containing a
--   field labeled with @f@, @p@ij is @y@ when @f@ labels the jth component
--   of @C@i or @_@ otherwise, and @e@i is @y@ when some field in @C@i has a
--   label of @f@ or @undefined@ otherwise.
dsRecField :: Name -> ExpQ
dsRecField = undefined


-- | This doesn't seem like a very useful de-sugaring to me, but it implements
--   the desugaring / translation of expression signatures specified in the
--   report:
--
-- @
--  e :: t   =  let { v :: t;  v = e } in v
-- @
dsSig :: Exp -> Type -> ExpQ
dsSig e t = do
  v <- newName "v"
  return [thExp'| let { $(v) :: $(t); $(v) = $(e) } in $(v) |]

-- | This doesn't seem like a very useful de-sugaring to me, but it implements
--   the desugaring / translation of lambdas specified in the report:
--
--   The following identity holds:
-- @
--   \p1 ... pn -> e  = \ x1 ... xn -> case (x1, ..., xn) of (p1, ..., pn) -> e
-- @
--   where the @xi@ are new identifiers.
--
--   This isn't very useful, because it still uses lambdas!
dsLambda :: [Pat] -> Exp -> ExpQ
dsLambda ps e
  | all isVar ps = return $ LamE ps e
  | otherwise = do
    names <- mapM (const $ newName "x") ps
    --TODO
    --return [thExp| \$(<x1 xn>map varP names) -> case ($(<x1, xn> map varE names)) of
    --               |]
    -- Parens used to prevent the de-sugaring from recursing forever.
    parensE . lamE (map varP names) 
            $ caseE (tupE $ map varE names) 
                    [return $ Match (TupP ps) (NormalB e) []]
 where
  isVar (VarP _) = True
  isVar _        = False

-}
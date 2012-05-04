{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
-- Based on description in http://www.haskell.org/onlinereport/exps.html
-- (Search for 'translation')

-- Not sure why TH doesn't provide
-- http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType

module Language.Haskell.TH.Desugar where

import Control.Monad         ( liftM )
import Control.Applicative   ( (<$>) )
import Data.Generics         ( Data )
import Data.Generics.Aliases ( extT, extM )
import Data.Generics.Schemes ( everywhere, everywhereM )
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.OverloadApp

eerror :: Either String a -> a
eerror (Left e) = error e
eerror (Right x) = x

allUInfix :: Exp -> Exp
allUInfix = everywhere (id `extT` convert)
 where
  convert (InfixE (Just l) o (Just r)) = UInfixE l o r
  convert e = e


desugar :: forall a. Data a => a -> Q a
desugar = everywhereM (return `extM` helper)
 where
  helper (InfixE    l o r) =            dsInfix    l o r
  helper (DoE          xs) = eerror <$> dsDo          xs
  helper (ListE        xs) =            dsList        xs
  helper (SigE        e t) =            dsSig        e t
  helper (LamE       ps e) =            dsLambda    ps e
  helper (ArithSeqE     r) =            dsArithSeq     r
  helper                e  =            return         e

dsInfix :: (Maybe Exp) -> Exp -> (Maybe Exp) -> ExpQ
dsInfix Nothing  o Nothing  = return o
dsInfix (Just l) o Nothing  = return $ o `AppE` l
dsInfix (Just l) o (Just r) = return $ o `AppE` l `AppE` r
dsInfix Nothing  o (Just r) = do
    x <- newName "x"
    return $ LamE [VarP x] (o `AppE` (VarE x) `AppE` r)

bindE :: ExpQ -> ExpQ -> ExpQ
bindE l r = infixE (Just l) (varE $ mkName ">>=") (Just r)

nclause :: [PatQ] -> ExpQ -> ClauseQ
nclause ps e = clause ps (normalB e) []

dsDo :: [Stmt] -> Q (Either String Exp)
dsDo [] = return $ Left "Empty 'do' block"

dsDo [NoBindS e] = return $ Right e

dsDo [e]         = return . Left
  $ "The last statement in a 'do' block must be an expression " ++ pprint e

dsDo (LetS ds:xs) = do
  rest <- dsDo xs
  return (LetE ds <$> rest)

dsDo (BindS p e:xs) = do
  result <- dsDo xs
  process result
 where
  process (Right rest) = do
    ok <- newName "ok"
    Right <$>
      letE [funD ok [ nclause [return p] (return rest)
                    , nclause [wildP] [| fail "Pattern match failure in do expression" |]
                    ]
           ]
           (bindE (return e) (varE ok))
  process l = return l

dsList :: [Exp] -> ExpQ
dsList = return . foldr (\l r -> AppE (AppE consE l) r) (ListE [])
 where
  consE = VarE $ mkName ":"


dsSig :: Exp -> Type -> ExpQ
dsSig e t = do
  v <- newName "v"
  letE [ sigD v (return t)
       , funD v [nclause [] $ return e] ]
       $ varE v

dsLambda :: [Pat] -> Exp -> ExpQ
dsLambda ps e = do
  names <- mapM (const $ newName "x") ps
  -- Parens used to prevent the de-sugaring from recursing forever.
  parensE . lamE (map varP names) 
          $ caseE (tupE $ map varE names) 
                  [match (return $ TupP ps) (normalB $ return e) []]

dsArithSeq :: Range -> ExpQ
dsArithSeq (FromR       f    ) = appE (varE $ mkName "enumFrom"      ) (return f)
dsArithSeq (FromThenR   f n  ) = appE (varE $ mkName "enumFromThen"  ) (return f)
dsArithSeq (FromToR     f   t) = appE (varE $ mkName "enumFromTo"    ) (return f)
dsArithSeq (FromThenToR f n t) = appE (varE $ mkName "enumFromThenTo") (return f)

--TODO: What is "ExplicitPArr", PArrSeq, etc?

{- TODO
dsComp :: [Stmt] -> ExpQ
dsComp xs = 

dsIf :: Exp -> Exp -> Exp -> ExpQ
dsIf c t e = 

dsRecCon :: Name -> [FieldExp] -> ExpQ
dsRecCon n fes = 

dsRecUpd :: Exp -> [FieldExp] -> ExpQ
dsRecUpd e fes = 
-}
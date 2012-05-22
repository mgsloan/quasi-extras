-- Based on http://hackage.haskell.org/trac/haskell-prime/wiki/FixityResolution
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Haskell.TH.Fixity where

import Control.Monad         ( liftM )
import Data.Generics         ( Data, gmapM, everywhereM, extM, everywhere, extT )
import Debug.Trace           ( trace )
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Language.Haskell.TH.Builders ( parseExp )

data Op = Op Exp Fixity
  deriving Eq

-- Input tokens for operator parsing
data OpTok = TExp Exp
           | TOp Op

-- Output with resolvd fixities
data OpFixed = OpInfix OpFixed Op OpFixed
             | OpExp Exp
  deriving (Eq, Show)

instance Show Op where
  show (Op name fix) = "'" ++ pprint name ++ "' [" ++ show fix ++ "]"

resolveToks :: [OpTok] -> Either String OpFixed
resolveToks = fmap fst . parseOps (Op (VarE $ mkName "dummy") $ Fixity (-1) InfixN)
 where
  parseOps :: Op -> [OpTok] -> Either String (OpFixed, [OpTok])
  parseOps op (TExp e1 : rest) = parseOp op (OpExp e1) rest
  parseOps _ (TOp name : _)
   = Left $ "Could not process leading operator '" ++ show name ++ "'"
  parseOps op []
   = Left $ "Expected expression after trailing operator " ++ show op

  parseOp :: Op -> OpFixed -> [OpTok] -> Either String (OpFixed, [OpTok])
  parseOp _   e1 [] = Right (e1, [])
  parseOp op1 e1 tokens@(TOp op2 : rest) 
    -- case (1): check for illegal expressions
    | prec1 == prec2 && (fix1 /= fix2 || fix1 == InfixN)
    = Left $ "Precedence parsing error: cannot mix "
           ++ show op1 ++ " and " ++ show op2
           ++ " in the same infix expression"

    -- case (2): op1 and op2 should associate to the left
    | prec1 > prec2 || (prec1 == prec2 && fix1 == InfixL)
    = Right (e1, tokens)

    -- case (3): op1 and op2 should associate to the right
    | otherwise
    = do (r, rest') <- parseOps op2 rest
         parseOp op1 (OpInfix e1 op2 r) rest'
   where
    Op _ (Fixity prec1 fix1) = op1
    Op _ (Fixity prec2 fix2) = op2
  parseOp _ _ (TExp e : _)
    = Left $ "Expected operator instead of " ++ pprint e

notfound :: Ppr a => a -> Fixity
notfound x = trace ("Warning: no fixity information found for " ++ pprint x)
                   defaultFixity

lookupFixity :: Name -> Q Fixity
lookupFixity n = recover (return $ notfound n) (process =<< reify n)
 where
  process (ClassOpI _ _ _ f) = return f
  process (DataConI _ _ _ f) = return f
  process (VarI     _ _ _ f) = return f
  process _ = return $ notfound n

getFixity :: Exp -> Q Fixity
getFixity (ConE n) = lookupFixity n
getFixity (VarE n) = lookupFixity n
getFixity e = return $ notfound e

toOpToks :: Monad m => (Exp -> m Fixity) -> Exp -> m [OpTok]
toOpToks f e = case e of
  ParensE e' -> toOpToks f e'
  _          -> helper e
 where
  helper (UInfixE l o r) = do
    l' <- helper l
    prec <- f o
    r' <- helper r
    return $ l' ++ [TOp $ Op o prec] ++ r'
  helper x = return [TExp x]

fromFixed :: OpFixed -> Exp
fromFixed (OpInfix l (Op e _) r)
  = InfixE (Just $ fromFixed l) e (Just $ fromFixed r)
fromFixed (OpExp e) = e

resolveTopFixities :: Exp -> ExpQ
resolveTopFixities = liftM (fromFixed . either error id . resolveToks)
                   . toOpToks getFixity

-- Should really be in syb..
everywhereM' :: (Data b, Monad m) => (forall a. Data a => a -> m a) -> b -> m b
everywhereM' f x = do x' <- f x
                      gmapM (everywhereM' f) x'

resolveFixities = everywhereM' (return `extM` resolveTopFixities)

-- Expression quasiquoter
equasi :: (String -> ExpQ) -> QuasiQuoter
equasi f = QuasiQuoter f undefined undefined undefined

allUInfix :: Data a => a -> a
allUInfix = everywhere (id `extT` convert)
 where
  convert (InfixE (Just l) o (Just r)) = UInfixE l o r
  convert e = e





{-
-- Test quasiquoter
fixityQ :: QuasiQuoter
fixityQ = equasi $ resolveFixities . parseExp

desugarQ :: QuasiQuoter
desugarQ = equasi ((desugar =<<) . resolveFixities . parseExp)
-}



{- Version that handles negation (unnecessary)

resolve tokens = fmap fst $ parseNeg (Op "" (-1) Nonfix) tokens
 where
  parseNeg :: Op -> [OpTok] -> Either String (OpFixed, [OpTok])
  parseNeg op1 (TExp e1 : rest) = parse op1 e1 rest
  parseNeg op1 (TOp name : rest)
    | name == mkName "GHC.Num.negate"
     = do guard (prec1 < 6)
         (r, rest') <- parseNeg (Op "-" 6 Leftfix) rest
         parse op1 (Neg r) rest'

    | otherwise
    = error $ "Could not process leading operator '" ++ name ++ "'"
   where
    Op _ prec1 fix1 = op1
-}
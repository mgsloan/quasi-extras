{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns, ScopedTypeVariables, TupleSections #-}

module Language.Quasi.Ast where

import Control.Applicative                ( (<$>) )
import Control.Arrow                      ( (***) )
import Control.Monad.Trans.Class          ( lift )
import Control.Monad.Trans.Either         ( EitherT(..), hoistEither )
import Data.Char                          ( isSpace )
import Data.Either                        ( rights )
import Data.List                          ( isPrefixOf, tails )
import Text.Themplates

type AstChunk = Chunk String (String, String)

parseAstQQ :: Monad m => String -> EitherT String m ([AstChunk], [String])
parseAstQQ input = do
  chunks <- hoistEither $ parseSplices curlySplice input
  let named = zipWith give_name (generateNames "splice" input) chunks

  return ( map process_free named
         , [x | s@(Right (x, _)) <- named, is_free s]
         )
 where
  give_name n (Right (Nothing, s)) = Right (n, s)
  give_name _ (Right (Just n, s))  = Right (n, s)
  give_name _ (Left str)           = Left str

  is_free = either (const False) (null . filter (not . isSpace) . snd)

  process_free s@(Right (n, _))
    | is_free s = Right (n, n)
    | otherwise = s
  process_free x = x

-- | Implements the generic process of converting a list of chunks into resulting code.
--   The first two parameters are the parsers used for the plain literal and the splice,
--   respectively.  The next two parameters provide how to convert the lift results (Exp)
--   into something that can be stored in the resulting splice type, and
{- doSplices :: forall a b. (TH.Lift a, Data b, Ord b)
          => (String -> EitherT String Q a) -> (String -> EitherT String Q b) -- Parsers
          -> (Exp -> Q b) -> (Exp -> b -> b)                                  -- Results processors
          -> [Chunk Char (String, String)] -> EitherT String Q b              -- Resulting Chunks-processor.
 -}

-- This function takes a parser and replicating pattern, and infers the
-- inductive structure used to convert to a list.
{-
parserListCons
  :: (Monad m, Data a)
  => (String -> EitherT String m a)
  -> (String, String, String)
  -> a 

parserListPat
  :: 
-}
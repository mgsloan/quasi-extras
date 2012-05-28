{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Language.Quasi.StringSub where

import Control.Arrow                   ( second )
import Control.Applicative             ( (<$>) )
import Data.List                       ( find, isPrefixOf, inits, tails )
import Text.Themplates                 ( parseSplices, curlySplice )
import Language.Haskell.TH
import Language.Haskell.TH.Lift        ( Lift(..), lift )
import Language.Haskell.TH.Quote       ( QuasiQuoter(..)  )
import Language.Quasi.Ast.TH           ( e', p' )
import Language.Quasi.Internal.Utils   ( fromLeft, parseExp, parsePat )
import System.IO.Unsafe                ( unsafePerformIO )

--TODO: handle escaping.

-- let name = "foobar" in [s| hello, {{ name }} |]
--
-- " hello, foobar "

splitFind :: String -> String -> Maybe (String, String)
splitFind on xs =   second (drop $ length on)
                <$> (find (isPrefixOf on . snd) $ zip (inits xs) (tails xs))

uLift :: Lift a => a -> Exp
uLift = uQ . lift

uQ :: Q a -> a
uQ = unsafePerformIO . runQ

s :: QuasiQuoter
s = QuasiQuoter expr pat undefined undefined
 where
  expr code
    = return
    . foldr [e'| {{}} ++ {{}} |] [e'| [] |]
    . map (either uLift (ParensE . fromLeft . parseExp . snd))
    . fromLeft
    $ parseSplices curlySplice code

  pat code = return $ case chunks of
    (Left c:cs) -> foldr (\ch -> [p'| {{LitP $ CharL ch}} : {{}} |])
                          (process cs) c
    cs          -> process cs
   where
    -- map (chunk (LitP . StringP) (ViewP . fromLeft . parsePat "" . snd))
    process []       = LitP $ StringL ""
    process [Left c] = LitP $ StringL c
    process (Right (_, splice) : Left c : cs)
      = [p'| (splitFind {{uLift c}}
                -> Just ( {{fromLeft $ parsePat splice}}
                        , {{process cs}}))
           |]
    process _ = error "Cannot handle adjacent splices in a string-search match quasi-quoter."
    chunks = fromLeft $ parseSplices curlySplice code
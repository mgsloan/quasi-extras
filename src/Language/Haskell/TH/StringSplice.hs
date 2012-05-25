module Language.Haskell.TH.StringSplice where

import Text.Themplates

--TODO: figure out how to escape splices

s :: QuasiQuoter
s = QuasiQuoter expr pat undefined undefined
 where
  expr code = do
    chunks <- parseSplices curlySplice code
    chunk (literalE . stringL) ()
  pat  = undefined
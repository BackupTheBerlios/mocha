module Mocha.THLibrary
(
   mkTuple
)

where

import Language.Haskell.THSyntax

mkTuple :: [Type] -> Type
mkTuple list = mkTuple' n n (reverse list)
   where
      n = length list
      mkTuple' :: Int -> Int -> [Type] -> Type
      mkTuple' 1 tupleLength (a:[]) = AppT (TupleT tupleLength) a
      mkTuple' n tupleLength (a:x) = AppT (mkTuple' (n-1) tupleLength x) a


{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

module Mocha.ReifyType

where

import Data.Dynamic
import Foreign (unsafePerformIO)
import Language.Haskell.THSyntax

class ReifyType a where
   rType :: a -> Type

instance ReifyType () where
   rType _ = TupleT 0

instance (ReifyType a, ReifyType b) => ReifyType (a, b) where
   rType (_ :: (t, u)) = AppT (AppT (TupleT 2) (rType (undefined :: t)))
			      (rType (undefined :: u))

instance ReifyType a => ReifyType [a] where
   rType (_ :: [t]) = AppT ListT (rType (undefined :: t))

instance ReifyType Char where
   rType _ = (ConT "GHC.Base:Char")

instance Typeable a => ReifyType a where
   rType (_ :: t) = (ConT typeAsString)
      where
	 shownTypeOf = show $ typeOf (undefined :: t)
	 typeAsString
	    -- Should at least have one for each of the ObjC types
	    -- which are marshalable by Mocha
	    | shownTypeOf == "Bool"     = "GHC.Base:Bool"
	    | shownTypeOf == "Char"     = "GHC.Base:Char"
	    | shownTypeOf == "Int"      = "GHC.Base:Int"
	    | shownTypeOf == "Float"    = "GHC.Float:Float"
	    -- XXX: Do we need the four lines above?
	    | otherwise			= replaceLastDotWithColon
					  shownTypeOf

instance (ReifyType a, ReifyType b) => ReifyType (a -> b) where
  rType (_ :: t -> u)
      = (AppT (AppT ArrowT (rType (undefined :: t))) (rType (undefined :: u)))

instance ReifyType a => ReifyType (IO a) where
  rType (_ :: IO b) = (AppT (ConT "GHC.IOBase:IO")
                       (rType (undefined :: b)))

-- XXX: How to handle other monads?

-- If anybody feels like replacing this with a slightly less stupid version,
-- please do! :)
replaceLastDotWithColon :: String -> String
replaceLastDotWithColon s
   | '.' `notElem` s = s
   | otherwise	     = untilLastDot ++ ":" ++ afterLastDot
   where
      afterLastDot = reverse $ takeWhile (/= '.') reversedString
      untilLastDot = reverse $ tail $ dropWhile (/= '.') reversedString
      reversedString = reverse s

instance ReifyType Type where
   rType = id

instance ReifyType ExpQ where
   rType (Q exp) = case (unsafePerformIO exp) of
      SigE _ t -> rType t
      -- _ -> error $ "Can't reifyType of ExpQ: (" ++ show exp ++ ")"


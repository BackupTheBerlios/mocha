{-# OPTIONS -fglasgow-exts #-}

module Mocha.TypeEncodings

where

import Data.Dynamic -- purely so we can use the 'typeOf' function
import Data.IORef
import Data.List (isPrefixOf)
import Foreign
import Foreign.C.Types
import Language.Haskell.THSyntax

import Mocha.Base
import Mocha.ReifyType

-- XXX: do we really need Show t here?
class ObjCTypeEncoding t where
   typeEncoding :: t -> String
   typeEncoding _ = "@" -- default for objects
--   decodeType :: String -> t
--   decodeType "@" = undefined :: ID

-- see
-- file:///Developer/Documentation/Cocoa/ObjectiveC/4objc_runtime_overview/Type_Encodings.html

instance ObjCTypeEncoding Bool	     where typeEncoding _ = "c"
--					   decodeType "c" 
instance ObjCTypeEncoding Char       where typeEncoding _ = "c"
instance ObjCTypeEncoding Int	     where typeEncoding _ = "i"
instance ObjCTypeEncoding CInt	     where typeEncoding _ = "i"
instance ObjCTypeEncoding CUInt      where typeEncoding _ = "I"
instance ObjCTypeEncoding CShort     where typeEncoding _ = "s"
instance ObjCTypeEncoding CUShort    where typeEncoding _ = "S"
instance ObjCTypeEncoding CLong      where typeEncoding _ = "l"
instance ObjCTypeEncoding CULong     where typeEncoding _ = "L"
instance ObjCTypeEncoding CLLong     where typeEncoding _ = "q"
instance ObjCTypeEncoding CULLong    where typeEncoding _ = "Q"
{-
instance ObjCTypeEncoding Float where typeEncoding _ = "f"
instance ObjCTypeEncoding Double where typeEncoding _ = "d"
instance ObjCTypeEncoding String where typeEncoding _ = "*"
-}

instance ObjCTypeEncoding ID         where typeEncoding _ = "@"
instance ObjCTypeEncoding ()         where typeEncoding _ = ""
instance ObjCTypeEncoding (Ptr x)    where typeEncoding _ = "^"
instance ObjCTypeEncoding (FunPtr x) where typeEncoding _ = "^?"
instance ObjCTypeEncoding Selector   where typeEncoding _ = "*"
   -- FIXME: Should really be ":"

instance (Typeable a, ObjCTypeEncoding a) => ObjCTypeEncoding (IO a) where
   -- scoped type variables are the bomb
   typeEncoding (x :: IO a)
      | typeOf x == typeOf (undefined :: IO ()) = "v"
      | otherwise				= typeEncoding (undefined :: a)

instance ObjCTypeEncoding ExpQ where
   typeEncoding (Q ioExp) = case (unsafePerformIO ioExp) of
      SigE _ t -> "@:" ++ typeEncoding t
      exp -> error $ "Can't reifyType of ExpQ: (" ++ show exp ++ ")"

instance ObjCTypeEncoding Type where
   typeEncoding (ConT "GHC.Base:Bool") = "c"
   typeEncoding (ConT "GHC.Base:Char") = "c"
   typeEncoding (ConT "GHC.Base:Int") = "i"
   typeEncoding (ConT "NSInvocation") = "@"
   typeEncoding (ConT "NSTextField") = "@"
   typeEncoding (ConT "GHCOB.IOBase:IO") = ""
   typeEncoding (ConT "GHC.Base:()") = ""
   typeEncoding (ConT typeConstructor)
      | "Mocha.ExportTemplates:" `isPrefixOf` typeConstructor = "@"
      | "Mocha.Base:" `isPrefixOf` typeConstructor = "@"
      | otherwise = error $ "typeEncoding doesn't understand the " ++
	 "type constructor \"" ++ typeConstructor ++ "\" yet"
   typeEncoding (AppT ArrowT t) = typeEncoding t
   typeEncoding (AppT t u) = typeEncoding t ++ typeEncoding u
   typeEncoding (TupleT 0) = ""

instance ObjCTypeEncoding TypeQ where
   typeEncoding (Q ioType) = "v@:" ++ typeEncoding t
      where t = unsafePerformIO ioType

{-
tAppToList (Tapp (Tcon Arrow) t) = tAppToList t
tAppToList (Tapp (Tcon (TconName "GHC.IOBase:IO")) t) = tAppToList t
tAppToList (Tapp t u) = tAppToList t ++ tAppToList u
tAppToList t = [t]
-}

instance (ObjCTypeEncoding t1, ObjCTypeEncoding t2) =>
         ObjCTypeEncoding (t1 -> t2) where
   -- Like i said, scoped type variables are the bomb!
   -- You could even use scoped type variables to implement a reifyType
   -- function ...
   typeEncoding (_ :: (t1 -> t2)) =
      typeEncoding (undefined :: t1) ++
      typeEncoding (undefined :: t2)

testFunction :: ID -> Bool -> Int -> IO ()
testFunction = undefined

typeEncodingForMethod :: ObjCTypeEncoding t => t -> String
typeEncodingForMethod f = [returnValue] ++ "@:" ++ arguments
   where
      primitiveTypeEncoding = typeEncoding f
      returnValue = last primitiveTypeEncoding
      arguments = init primitiveTypeEncoding

-- XXX: Can we tidy this up a bit?
instance (Typeable a) => ObjCTypeEncoding [a] where
   typeEncoding list
      | typeOf list == typeOf (undefined :: String) = "@"
        -- we will marshal a string
      | otherwise				    = "[]"

instance ObjCTypeEncoding ref =>
         ObjCTypeEncoding (IORef ref) where
   typeEncoding ref = unsafePerformIO $ do
      r <- readIORef ref
      return (typeEncoding r)

-- Short
-- Long
-- Long Long
-- Unsigned Char
-- Unsigned int
-- unsigned short
-- unsigned long long

{-
instance ObjCTypeEncoding ObjCInstanceObject where typeEncoding _ = "@"
instance ObjCTypeEncoding ObjCClassObject where typeEncoding _ = "#"
instance ObjCTypeEncoding ObjCMetaclassObject where typeEncoding _ = "@" -- or #?
-}


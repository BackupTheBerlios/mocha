{-# OPTIONS -fglasgow-exts #-}

module Mocha.Base

where

import Control.Monad
import Data.Dynamic hiding (cast)
import Foreign.C.String
import Foreign.Ptr

import GHC.IOBase

--- UPCASTING/DOWNCASTING ---

class Cast from to where
   cast :: from -> to

class Cast super sub => Downcast super sub
class (Cast sub super, Downcast super sub) => Upcast sub super

--- OBJECTIVE-C CLASS HIERACHY ---

type ClassName = String

newtype ID = ID (Ptr ID)
   deriving (Eq, Show, Typeable)

class (Downcast ID o, Upcast o ID) => ObjCObject o

instance ObjCObject ID

instance Cast ID ID where
   cast = id
instance Downcast ID ID
instance Upcast ID ID

-- Class Hierachy

newtype ObjCClassObject = ObjCClassObject (Ptr ObjCClassObject)
  deriving (Eq, Show)

class (Upcast o ObjCClassObject, Downcast ObjCClassObject o, ObjCObject o)
   => ObjCClass o where
   classObjectName :: o -> String

instance ObjCObject ObjCClassObject
instance ObjCClass ObjCClassObject where
   classObjectName _ = "Object"
instance Cast ObjCClassObject ID where
   cast (ObjCClassObject p) = ID (castPtr p)
instance Cast ID ObjCClassObject where
   cast (ID p) = ObjCClassObject (castPtr p)
instance Downcast ID ObjCClassObject
instance Upcast ObjCClassObject ID
instance Downcast ObjCClassObject ObjCClassObject
instance Upcast ObjCClassObject ObjCClassObject
instance Cast ObjCClassObject ObjCClassObject where
   cast = id

-- Metaclass Hierachy

newtype ObjCMetaclassObject = ObjCMetaclassObject (Ptr ObjCMetaclassObject)
  deriving (Eq, Show)

class (ObjCObject o, Cast o ObjCMetaclassObject) => ObjCMetaclass o where
   metaclassObjectName :: o -> String

instance ObjCObject ObjCMetaclassObject
instance Cast ObjCMetaclassObject ID where
   cast (ObjCMetaclassObject p) = ID (castPtr p)
instance Cast ID ObjCMetaclassObject where
   cast (ID p) = ObjCMetaclassObject (castPtr p)
instance Downcast ID ObjCMetaclassObject
instance Upcast ObjCMetaclassObject ID
instance Downcast ObjCMetaclassObject ObjCMetaclassObject
instance Upcast ObjCMetaclassObject ObjCMetaclassObject
instance ObjCMetaclass ObjCMetaclassObject where
   metaclassObjectName _ = "Object"
instance Cast ObjCMetaclassObject ObjCMetaclassObject where
   cast = id

-- Instance Hierachy

newtype ObjCInstanceObject = ObjCInstanceObject (Ptr ObjCInstanceObject)
   deriving (Eq, Show)

class (ObjCObject o, Cast o ObjCInstanceObject) => ObjCInstance o where
   getClassObjectM :: (Cast ObjCClassObject o2, ObjCClass o2) => o -> IO o2

instance ObjCObject ObjCInstanceObject
instance Cast ObjCInstanceObject ID where
   cast (ObjCInstanceObject p) = ID (castPtr p)
instance Cast ID ObjCInstanceObject where
   cast (ID p) = ObjCInstanceObject (castPtr p)
instance Downcast ID ObjCInstanceObject
instance Upcast ObjCInstanceObject ID
instance ObjCInstance ObjCInstanceObject where
   getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Downcast ObjCInstanceObject ObjCInstanceObject
instance Upcast ObjCInstanceObject ObjCInstanceObject
instance Cast ObjCInstanceObject ObjCInstanceObject where
   cast = id

infixl 1 >>=^
(>>=^) beforeAction afterAction = liftM afterAction beforeAction

-- The "null" object (pointer to NULL)
nil :: ID
nil = ID (nullPtr :: Ptr ID)

-- The "null" class object
_Nil_ :: ObjCClassObject
_Nil_ = ObjCClassObject (nullPtr :: Ptr ObjCClassObject)

_nil_ :: ObjCClassObject
_nil_ = _Nil_

-- The "null" metaclass object
__Nil__ :: ObjCMetaclassObject
__Nil__ = ObjCMetaclassObject (nullPtr :: Ptr ObjCMetaclassObject)

__nil__ :: ObjCMetaclassObject
__nil__ = __Nil__


getClassObject :: (Cast ObjCClassObject b, ObjCClass b) => b
getClassObject =
   let 
      x = cast $ getClassObjectFromName (classObjectName x)
   in
      x

getClassObjectFromName :: String -> ObjCClassObject
getClassObjectFromName className =
   objc_getClass $ unsafePerformIO (newCString className)

getClassObjectFromNameM :: String -> IO ObjCClassObject
getClassObjectFromNameM className = withCString className objc_getClassM


-- Selectors

newtype Selector = Selector String
   deriving (Eq, Show, Typeable)

type SEL = Selector

sel_getName :: Selector -> String
sel_getName (Selector s) = s

-- We should really declare that objc_getClass is located in objc-runtime.h,
-- but objc-runtime.h has Objective-C syntax and gcc will try to compile it
-- as a standard C program since it has the usual .h extension.  Putting
-- "-optc -x -optc objective-c" or "-optc -ObjC" on the GHC commandline
-- doesn't help, since GHC will pass those options to GHC as the _last_ things
-- on the commandline, after the include file.

foreign import ccall unsafe "objc_getClass.h objc_getClass"
   objc_getClass :: CString -> ObjCClassObject

foreign import ccall unsafe "objc_getClass.h objc_getClass"
   objc_getClassM :: CString -> IO ObjCClassObject


{-# OPTIONS -fglasgow-exts #-}

module Mocha.ClassDefinition where

-- from HOC.hs

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

data ObjCClassDefinition = ObjCClassDefinition
   (Ptr ObjCClassDefinition) -- ^ metaclass
   (Ptr ObjCClassDefinition) -- ^ superclass
   CString -- ^ classname
   CLong -- ^ version
   CLong -- ^ info
   CLong -- ^ instance size
   (Ptr ()) -- ^ instance variables (ignore)
   (Ptr ObjCMethodList) -- ^ method list
   (Ptr ()) -- ^ cache (ignore)
   (Ptr ()) -- ^ protocol (FIXME: shouldn't ignore)


data ObjCMethodList


{-
type HOCivar = (String, String, CInt)

data ObjCInstanceVariable = ObjCInstanceVariable CString CString CInt

instance Storable ObjCInstanceVariable where
   sizeOf _ = 3 * sizeOf (nullPtr)
   alignment _ = alignment nullPtr
   poke p (ObjCInstanceVariable name typeEncoding offsetInClass) = do
      pokeElemOff (castPtr p) 0 name
      pokeElemOff (castPtr p) 1 typeEncoding
      pokeElemOff (castPtr p) 2 offsetInClass
   peek p = do
      name <- peek (castPtr p) :: IO CString
      typeEncoding <- peekByteOff (castPtr p) sizeOfCString :: IO CString
      offsetInClass <- peekByteOff (castPtr p) (sizeOfCString * 2) :: IO CInt
      return $ ObjCInstanceVariable name typeEncoding offsetInClass
      where
	 sizeOfCString = sizeOf (undefined :: CString)


{-
data ObjCInstanceVariableList = ObjCInstanceVariableList
				[ObjCInstanceVariable]

instance Storable ObjCInstanceVariableList where
   sizeOf (ObjCInstanceVariableList instanceVariables) =
      sizeOf (undefined :: CInt) +
      length instanceVariables * sizeOf (ObjCInstanceVariable)
   poke p (ObjCInstanceVariableList instanceVariables) =
      pokeElemOff (castPtr p :: Ptr CInt) 0
-}

newIvarList :: [ObjCInstanceVariable] -> IO (Ptr ())
newIvarList ivarsC = do
   ivarList <- mallocBytes
      (sizeOf (undefined :: CInt) + length ivarsC * sizeOf (head ivarsC))
   poke ivarList (fromIntegral (length ivarsC) :: CInt)
   let varr = ivarList `plusPtr` (sizeOf (0 :: CInt))
   pokeArray (castPtr varr) ivarsC
   return $ castPtr ivarList

{-
foreign import ccall "class_getInstanceVariable"
   cGetInstanceVariable :: ObjCClassObject -> CString -> Ptr 
-}

-}


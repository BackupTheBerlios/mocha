{-# OPTIONS -fglasgow-exts #-}
module Nyah where
import Mocha.Base
import Foreign.Ptr
import Mocha.Messaging
import Mocha.TypeEncodings
newtype NSObjectMetaclassObject = NSObjectMetaclassObject (Ptr NSObjectMetaclassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSObjectMetaclassObject, Downcast NSObjectMetaclassObject
						c) => NSObjectMetaclass c
instance NSObjectMetaclass NSObjectMetaclassObject
instance Cast NSObjectMetaclassObject NSObjectMetaclassObject
    where cast = id
instance Upcast NSObjectMetaclassObject NSObjectMetaclassObject
instance Downcast NSObjectMetaclassObject NSObjectMetaclassObject
newtype NSArrayMetaclassObject = NSArrayMetaclassObject (Ptr NSArrayMetaclassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSArrayMetaclassObject, Downcast NSArrayMetaclassObject
					       c, NSObjectMetaclass c) => NSArrayMetaclass c
instance NSArrayMetaclass NSArrayMetaclassObject
instance Cast NSArrayMetaclassObject NSArrayMetaclassObject
    where cast = id
instance Upcast NSArrayMetaclassObject NSArrayMetaclassObject
instance Downcast NSArrayMetaclassObject NSArrayMetaclassObject
instance NSObjectMetaclass NSArrayMetaclassObject
instance Cast NSArrayMetaclassObject NSObjectMetaclassObject
    where cast (NSArrayMetaclassObject p) = NSObjectMetaclassObject (castPtr p)
instance Cast NSObjectMetaclassObject NSArrayMetaclassObject
    where cast (NSObjectMetaclassObject p) = NSArrayMetaclassObject (castPtr p)
instance Upcast NSArrayMetaclassObject NSObjectMetaclassObject
instance Downcast NSObjectMetaclassObject NSArrayMetaclassObject
newtype NSDictionaryMetaclassObject = NSDictionaryMetaclassObject (Ptr NSDictionaryMetaclassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSDictionaryMetaclassObject, Downcast NSDictionaryMetaclassObject
						    c, NSObjectMetaclass c) => NSDictionaryMetaclass c
instance NSDictionaryMetaclass NSDictionaryMetaclassObject
instance Cast NSDictionaryMetaclassObject
	      NSDictionaryMetaclassObject
    where cast = id
instance Upcast NSDictionaryMetaclassObject
		NSDictionaryMetaclassObject
instance Downcast NSDictionaryMetaclassObject
		  NSDictionaryMetaclassObject
instance NSObjectMetaclass NSDictionaryMetaclassObject
instance Cast NSDictionaryMetaclassObject NSObjectMetaclassObject
    where cast (NSDictionaryMetaclassObject p) = NSObjectMetaclassObject (castPtr p)
instance Cast NSObjectMetaclassObject NSDictionaryMetaclassObject
    where cast (NSObjectMetaclassObject p) = NSDictionaryMetaclassObject (castPtr p)
instance Upcast NSDictionaryMetaclassObject NSObjectMetaclassObject
instance Downcast NSObjectMetaclassObject
		  NSDictionaryMetaclassObject
newtype NSInvocationMetaclassObject = NSInvocationMetaclassObject (Ptr NSInvocationMetaclassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSInvocationMetaclassObject, Downcast NSInvocationMetaclassObject
						    c, NSObjectMetaclass c) => NSInvocationMetaclass c
instance NSInvocationMetaclass NSInvocationMetaclassObject
instance Cast NSInvocationMetaclassObject
	      NSInvocationMetaclassObject
    where cast = id
instance Upcast NSInvocationMetaclassObject
		NSInvocationMetaclassObject
instance Downcast NSInvocationMetaclassObject
		  NSInvocationMetaclassObject
instance NSObjectMetaclass NSInvocationMetaclassObject
instance Cast NSInvocationMetaclassObject NSObjectMetaclassObject
    where cast (NSInvocationMetaclassObject p) = NSObjectMetaclassObject (castPtr p)
instance Cast NSObjectMetaclassObject NSInvocationMetaclassObject
    where cast (NSObjectMetaclassObject p) = NSInvocationMetaclassObject (castPtr p)
instance Upcast NSInvocationMetaclassObject NSObjectMetaclassObject
instance Downcast NSObjectMetaclassObject
		  NSInvocationMetaclassObject
newtype NSMethodSignatureMetaclassObject = NSMethodSignatureMetaclassObject (Ptr NSMethodSignatureMetaclassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSMethodSignatureMetaclassObject, Downcast NSMethodSignatureMetaclassObject
							 c, NSObjectMetaclass c) => NSMethodSignatureMetaclass c
instance NSMethodSignatureMetaclass NSMethodSignatureMetaclassObject
instance Cast NSMethodSignatureMetaclassObject
	      NSMethodSignatureMetaclassObject
    where cast = id
instance Upcast NSMethodSignatureMetaclassObject
		NSMethodSignatureMetaclassObject
instance Downcast NSMethodSignatureMetaclassObject
		  NSMethodSignatureMetaclassObject
instance NSObjectMetaclass NSMethodSignatureMetaclassObject
instance Cast NSMethodSignatureMetaclassObject
	      NSObjectMetaclassObject
    where cast (NSMethodSignatureMetaclassObject p) = NSObjectMetaclassObject (castPtr p)
instance Cast NSObjectMetaclassObject
	      NSMethodSignatureMetaclassObject
    where cast (NSObjectMetaclassObject p) = NSMethodSignatureMetaclassObject (castPtr p)
instance Upcast NSMethodSignatureMetaclassObject
		NSObjectMetaclassObject
instance Downcast NSObjectMetaclassObject
		  NSMethodSignatureMetaclassObject
newtype NSMovieMetaclassObject = NSMovieMetaclassObject (Ptr NSMovieMetaclassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSMovieMetaclassObject, Downcast NSMovieMetaclassObject
					       c, NSObjectMetaclass c) => NSMovieMetaclass c
instance NSMovieMetaclass NSMovieMetaclassObject
instance Cast NSMovieMetaclassObject NSMovieMetaclassObject
    where cast = id
instance Upcast NSMovieMetaclassObject NSMovieMetaclassObject
instance Downcast NSMovieMetaclassObject NSMovieMetaclassObject
instance NSObjectMetaclass NSMovieMetaclassObject
instance Cast NSMovieMetaclassObject NSObjectMetaclassObject
    where cast (NSMovieMetaclassObject p) = NSObjectMetaclassObject (castPtr p)
instance Cast NSObjectMetaclassObject NSMovieMetaclassObject
    where cast (NSObjectMetaclassObject p) = NSMovieMetaclassObject (castPtr p)
instance Upcast NSMovieMetaclassObject NSObjectMetaclassObject
instance Downcast NSObjectMetaclassObject NSMovieMetaclassObject
newtype NSMovieViewMetaclassObject = NSMovieViewMetaclassObject (Ptr NSMovieViewMetaclassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSMovieViewMetaclassObject, Downcast NSMovieViewMetaclassObject
						   c, NSViewMetaclass c) => NSMovieViewMetaclass c
instance NSMovieViewMetaclass NSMovieViewMetaclassObject
instance Cast NSMovieViewMetaclassObject NSMovieViewMetaclassObject
    where cast = id
instance Upcast NSMovieViewMetaclassObject
		NSMovieViewMetaclassObject
instance Downcast NSMovieViewMetaclassObject
		  NSMovieViewMetaclassObject
instance NSViewMetaclass NSMovieViewMetaclassObject
instance Cast NSMovieViewMetaclassObject NSViewMetaclassObject
    where cast (NSMovieViewMetaclassObject p) = NSViewMetaclassObject (castPtr p)
instance Cast NSViewMetaclassObject NSMovieViewMetaclassObject
    where cast (NSViewMetaclassObject p) = NSMovieViewMetaclassObject (castPtr p)
instance Upcast NSMovieViewMetaclassObject NSViewMetaclassObject
instance Downcast NSViewMetaclassObject NSMovieViewMetaclassObject
instance NSResponderMetaclass NSMovieViewMetaclassObject
instance Cast NSMovieViewMetaclassObject NSResponderMetaclassObject
    where cast (NSMovieViewMetaclassObject p) = NSResponderMetaclassObject (castPtr p)
instance Cast NSResponderMetaclassObject NSMovieViewMetaclassObject
    where cast (NSResponderMetaclassObject p) = NSMovieViewMetaclassObject (castPtr p)
instance Upcast NSMovieViewMetaclassObject
		NSResponderMetaclassObject
instance Downcast NSResponderMetaclassObject
		  NSMovieViewMetaclassObject
instance NSObjectMetaclass NSMovieViewMetaclassObject
instance Cast NSMovieViewMetaclassObject NSObjectMetaclassObject
    where cast (NSMovieViewMetaclassObject p) = NSObjectMetaclassObject (castPtr p)
instance Cast NSObjectMetaclassObject NSMovieViewMetaclassObject
    where cast (NSObjectMetaclassObject p) = NSMovieViewMetaclassObject (castPtr p)
instance Upcast NSMovieViewMetaclassObject NSObjectMetaclassObject
instance Downcast NSObjectMetaclassObject
		  NSMovieViewMetaclassObject
newtype NSNumberMetaclassObject = NSNumberMetaclassObject (Ptr NSNumberMetaclassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSNumberMetaclassObject, Downcast NSNumberMetaclassObject
						c, NSValueMetaclass c) => NSNumberMetaclass c
instance NSNumberMetaclass NSNumberMetaclassObject
instance Cast NSNumberMetaclassObject NSNumberMetaclassObject
    where cast = id
instance Upcast NSNumberMetaclassObject NSNumberMetaclassObject
instance Downcast NSNumberMetaclassObject NSNumberMetaclassObject
instance NSValueMetaclass NSNumberMetaclassObject
instance Cast NSNumberMetaclassObject NSValueMetaclassObject
    where cast (NSNumberMetaclassObject p) = NSValueMetaclassObject (castPtr p)
instance Cast NSValueMetaclassObject NSNumberMetaclassObject
    where cast (NSValueMetaclassObject p) = NSNumberMetaclassObject (castPtr p)
instance Upcast NSNumberMetaclassObject NSValueMetaclassObject
instance Downcast NSValueMetaclassObject NSNumberMetaclassObject
instance NSObjectMetaclass NSNumberMetaclassObject
instance Cast NSNumberMetaclassObject NSObjectMetaclassObject
    where cast (NSNumberMetaclassObject p) = NSObjectMetaclassObject (castPtr p)
instance Cast NSObjectMetaclassObject NSNumberMetaclassObject
    where cast (NSObjectMetaclassObject p) = NSNumberMetaclassObject (castPtr p)
instance Upcast NSNumberMetaclassObject NSObjectMetaclassObject
instance Downcast NSObjectMetaclassObject NSNumberMetaclassObject
newtype NSResponderMetaclassObject = NSResponderMetaclassObject (Ptr NSResponderMetaclassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSResponderMetaclassObject, Downcast NSResponderMetaclassObject
						   c, NSObjectMetaclass c) => NSResponderMetaclass c
instance NSResponderMetaclass NSResponderMetaclassObject
instance Cast NSResponderMetaclassObject NSResponderMetaclassObject
    where cast = id
instance Upcast NSResponderMetaclassObject
		NSResponderMetaclassObject
instance Downcast NSResponderMetaclassObject
		  NSResponderMetaclassObject
instance NSObjectMetaclass NSResponderMetaclassObject
instance Cast NSResponderMetaclassObject NSObjectMetaclassObject
    where cast (NSResponderMetaclassObject p) = NSObjectMetaclassObject (castPtr p)
instance Cast NSObjectMetaclassObject NSResponderMetaclassObject
    where cast (NSObjectMetaclassObject p) = NSResponderMetaclassObject (castPtr p)
instance Upcast NSResponderMetaclassObject NSObjectMetaclassObject
instance Downcast NSObjectMetaclassObject
		  NSResponderMetaclassObject
newtype NSStringMetaclassObject = NSStringMetaclassObject (Ptr NSStringMetaclassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSStringMetaclassObject, Downcast NSStringMetaclassObject
						c, NSObjectMetaclass c) => NSStringMetaclass c
instance NSStringMetaclass NSStringMetaclassObject
instance Cast NSStringMetaclassObject NSStringMetaclassObject
    where cast = id
instance Upcast NSStringMetaclassObject NSStringMetaclassObject
instance Downcast NSStringMetaclassObject NSStringMetaclassObject
instance NSObjectMetaclass NSStringMetaclassObject
instance Cast NSStringMetaclassObject NSObjectMetaclassObject
    where cast (NSStringMetaclassObject p) = NSObjectMetaclassObject (castPtr p)
instance Cast NSObjectMetaclassObject NSStringMetaclassObject
    where cast (NSObjectMetaclassObject p) = NSStringMetaclassObject (castPtr p)
instance Upcast NSStringMetaclassObject NSObjectMetaclassObject
instance Downcast NSObjectMetaclassObject NSStringMetaclassObject
newtype NSTextMetaclassObject = NSTextMetaclassObject (Ptr NSTextMetaclassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSTextMetaclassObject, Downcast NSTextMetaclassObject
					      c, NSViewMetaclass c) => NSTextMetaclass c
instance NSTextMetaclass NSTextMetaclassObject
instance Cast NSTextMetaclassObject NSTextMetaclassObject
    where cast = id
instance Upcast NSTextMetaclassObject NSTextMetaclassObject
instance Downcast NSTextMetaclassObject NSTextMetaclassObject
instance NSViewMetaclass NSTextMetaclassObject
instance Cast NSTextMetaclassObject NSViewMetaclassObject
    where cast (NSTextMetaclassObject p) = NSViewMetaclassObject (castPtr p)
instance Cast NSViewMetaclassObject NSTextMetaclassObject
    where cast (NSViewMetaclassObject p) = NSTextMetaclassObject (castPtr p)
instance Upcast NSTextMetaclassObject NSViewMetaclassObject
instance Downcast NSViewMetaclassObject NSTextMetaclassObject
instance NSResponderMetaclass NSTextMetaclassObject
instance Cast NSTextMetaclassObject NSResponderMetaclassObject
    where cast (NSTextMetaclassObject p) = NSResponderMetaclassObject (castPtr p)
instance Cast NSResponderMetaclassObject NSTextMetaclassObject
    where cast (NSResponderMetaclassObject p) = NSTextMetaclassObject (castPtr p)
instance Upcast NSTextMetaclassObject NSResponderMetaclassObject
instance Downcast NSResponderMetaclassObject NSTextMetaclassObject
instance NSObjectMetaclass NSTextMetaclassObject
instance Cast NSTextMetaclassObject NSObjectMetaclassObject
    where cast (NSTextMetaclassObject p) = NSObjectMetaclassObject (castPtr p)
instance Cast NSObjectMetaclassObject NSTextMetaclassObject
    where cast (NSObjectMetaclassObject p) = NSTextMetaclassObject (castPtr p)
instance Upcast NSTextMetaclassObject NSObjectMetaclassObject
instance Downcast NSObjectMetaclassObject NSTextMetaclassObject
newtype NSTextFieldMetaclassObject = NSTextFieldMetaclassObject (Ptr NSTextFieldMetaclassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSTextFieldMetaclassObject, Downcast NSTextFieldMetaclassObject
						   c, NSTextMetaclass c) => NSTextFieldMetaclass c
instance NSTextFieldMetaclass NSTextFieldMetaclassObject
instance Cast NSTextFieldMetaclassObject NSTextFieldMetaclassObject
    where cast = id
instance Upcast NSTextFieldMetaclassObject
		NSTextFieldMetaclassObject
instance Downcast NSTextFieldMetaclassObject
		  NSTextFieldMetaclassObject
instance NSTextMetaclass NSTextFieldMetaclassObject
instance Cast NSTextFieldMetaclassObject NSTextMetaclassObject
    where cast (NSTextFieldMetaclassObject p) = NSTextMetaclassObject (castPtr p)
instance Cast NSTextMetaclassObject NSTextFieldMetaclassObject
    where cast (NSTextMetaclassObject p) = NSTextFieldMetaclassObject (castPtr p)
instance Upcast NSTextFieldMetaclassObject NSTextMetaclassObject
instance Downcast NSTextMetaclassObject NSTextFieldMetaclassObject
instance NSViewMetaclass NSTextFieldMetaclassObject
instance Cast NSTextFieldMetaclassObject NSViewMetaclassObject
    where cast (NSTextFieldMetaclassObject p) = NSViewMetaclassObject (castPtr p)
instance Cast NSViewMetaclassObject NSTextFieldMetaclassObject
    where cast (NSViewMetaclassObject p) = NSTextFieldMetaclassObject (castPtr p)
instance Upcast NSTextFieldMetaclassObject NSViewMetaclassObject
instance Downcast NSViewMetaclassObject NSTextFieldMetaclassObject
instance NSResponderMetaclass NSTextFieldMetaclassObject
instance Cast NSTextFieldMetaclassObject NSResponderMetaclassObject
    where cast (NSTextFieldMetaclassObject p) = NSResponderMetaclassObject (castPtr p)
instance Cast NSResponderMetaclassObject NSTextFieldMetaclassObject
    where cast (NSResponderMetaclassObject p) = NSTextFieldMetaclassObject (castPtr p)
instance Upcast NSTextFieldMetaclassObject
		NSResponderMetaclassObject
instance Downcast NSResponderMetaclassObject
		  NSTextFieldMetaclassObject
instance NSObjectMetaclass NSTextFieldMetaclassObject
instance Cast NSTextFieldMetaclassObject NSObjectMetaclassObject
    where cast (NSTextFieldMetaclassObject p) = NSObjectMetaclassObject (castPtr p)
instance Cast NSObjectMetaclassObject NSTextFieldMetaclassObject
    where cast (NSObjectMetaclassObject p) = NSTextFieldMetaclassObject (castPtr p)
instance Upcast NSTextFieldMetaclassObject NSObjectMetaclassObject
instance Downcast NSObjectMetaclassObject
		  NSTextFieldMetaclassObject
newtype NSURLMetaclassObject = NSURLMetaclassObject (Ptr NSURLMetaclassObject)
    deriving (Eq, Show)
class (Upcast c NSURLMetaclassObject, Downcast NSURLMetaclassObject
					       c, NSObjectMetaclass c) => NSURLMetaclass c
instance NSURLMetaclass NSURLMetaclassObject
instance Cast NSURLMetaclassObject NSURLMetaclassObject
    where cast = id
instance Upcast NSURLMetaclassObject NSURLMetaclassObject
instance Downcast NSURLMetaclassObject NSURLMetaclassObject
instance NSObjectMetaclass NSURLMetaclassObject
instance Cast NSURLMetaclassObject NSObjectMetaclassObject
    where cast (NSURLMetaclassObject p) = NSObjectMetaclassObject (castPtr p)
instance Cast NSObjectMetaclassObject NSURLMetaclassObject
    where cast (NSObjectMetaclassObject p) = NSURLMetaclassObject (castPtr p)
instance Upcast NSURLMetaclassObject NSObjectMetaclassObject
instance Downcast NSObjectMetaclassObject NSURLMetaclassObject
newtype NSValueMetaclassObject = NSValueMetaclassObject (Ptr NSValueMetaclassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSValueMetaclassObject, Downcast NSValueMetaclassObject
					       c, NSObjectMetaclass c) => NSValueMetaclass c
instance NSValueMetaclass NSValueMetaclassObject
instance Cast NSValueMetaclassObject NSValueMetaclassObject
    where cast = id
instance Upcast NSValueMetaclassObject NSValueMetaclassObject
instance Downcast NSValueMetaclassObject NSValueMetaclassObject
instance NSObjectMetaclass NSValueMetaclassObject
instance Cast NSValueMetaclassObject NSObjectMetaclassObject
    where cast (NSValueMetaclassObject p) = NSObjectMetaclassObject (castPtr p)
instance Cast NSObjectMetaclassObject NSValueMetaclassObject
    where cast (NSObjectMetaclassObject p) = NSValueMetaclassObject (castPtr p)
instance Upcast NSValueMetaclassObject NSObjectMetaclassObject
instance Downcast NSObjectMetaclassObject NSValueMetaclassObject
newtype NSViewMetaclassObject = NSViewMetaclassObject (Ptr NSViewMetaclassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSViewMetaclassObject, Downcast NSViewMetaclassObject
					      c, NSResponderMetaclass c) => NSViewMetaclass c
instance NSViewMetaclass NSViewMetaclassObject
instance Cast NSViewMetaclassObject NSViewMetaclassObject
    where cast = id
instance Upcast NSViewMetaclassObject NSViewMetaclassObject
instance Downcast NSViewMetaclassObject NSViewMetaclassObject
instance NSResponderMetaclass NSViewMetaclassObject
instance Cast NSViewMetaclassObject NSResponderMetaclassObject
    where cast (NSViewMetaclassObject p) = NSResponderMetaclassObject (castPtr p)
instance Cast NSResponderMetaclassObject NSViewMetaclassObject
    where cast (NSResponderMetaclassObject p) = NSViewMetaclassObject (castPtr p)
instance Upcast NSViewMetaclassObject NSResponderMetaclassObject
instance Downcast NSResponderMetaclassObject NSViewMetaclassObject
instance NSObjectMetaclass NSViewMetaclassObject
instance Cast NSViewMetaclassObject NSObjectMetaclassObject
    where cast (NSViewMetaclassObject p) = NSObjectMetaclassObject (castPtr p)
instance Cast NSObjectMetaclassObject NSViewMetaclassObject
    where cast (NSObjectMetaclassObject p) = NSViewMetaclassObject (castPtr p)
instance Upcast NSViewMetaclassObject NSObjectMetaclassObject
instance Downcast NSObjectMetaclassObject NSViewMetaclassObject
newtype HSProxyMetaclassObject = HSProxyMetaclassObject (Ptr HSProxyMetaclassObject)
    deriving (Eq, Show)
class (Upcast c
	      HSProxyMetaclassObject, Downcast HSProxyMetaclassObject
					       c, NSObjectMetaclass c) => HSProxyMetaclass c
instance HSProxyMetaclass HSProxyMetaclassObject
instance Cast HSProxyMetaclassObject HSProxyMetaclassObject
    where cast = id
instance Upcast HSProxyMetaclassObject HSProxyMetaclassObject
instance Downcast HSProxyMetaclassObject HSProxyMetaclassObject
instance NSObjectMetaclass HSProxyMetaclassObject
instance Cast HSProxyMetaclassObject NSObjectMetaclassObject
    where cast (HSProxyMetaclassObject p) = NSObjectMetaclassObject (castPtr p)
instance Cast NSObjectMetaclassObject HSProxyMetaclassObject
    where cast (NSObjectMetaclassObject p) = HSProxyMetaclassObject (castPtr p)
instance Upcast HSProxyMetaclassObject NSObjectMetaclassObject
instance Downcast NSObjectMetaclassObject HSProxyMetaclassObject
newtype NSObjectClassObject = NSObjectClassObject (Ptr NSObjectClassObject)
    deriving (Eq, Show)
class (Upcast c NSObjectClassObject, Downcast NSObjectClassObject
					      c) => NSObjectClassD c
instance NSObjectClassD NSObjectClassObject
instance Cast NSObjectClassObject NSObjectClassObject
    where cast = id
instance Upcast NSObjectClassObject NSObjectClassObject
instance Downcast NSObjectClassObject NSObjectClassObject
newtype NSArrayClassObject = NSArrayClassObject (Ptr NSArrayClassObject)
    deriving (Eq, Show)
class (Upcast c NSArrayClassObject, Downcast NSArrayClassObject
					     c, NSObjectClassD c) => NSArrayClassD c
instance NSArrayClassD NSArrayClassObject
instance Cast NSArrayClassObject NSArrayClassObject
    where cast = id
instance Upcast NSArrayClassObject NSArrayClassObject
instance Downcast NSArrayClassObject NSArrayClassObject
instance NSObjectClassD NSArrayClassObject
instance Cast NSArrayClassObject NSObjectClassObject
    where cast (NSArrayClassObject p) = NSObjectClassObject (castPtr p)
instance Cast NSObjectClassObject NSArrayClassObject
    where cast (NSObjectClassObject p) = NSArrayClassObject (castPtr p)
instance Upcast NSArrayClassObject NSObjectClassObject
instance Downcast NSObjectClassObject NSArrayClassObject
newtype NSDictionaryClassObject = NSDictionaryClassObject (Ptr NSDictionaryClassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSDictionaryClassObject, Downcast NSDictionaryClassObject
						c, NSObjectClassD c) => NSDictionaryClassD c
instance NSDictionaryClassD NSDictionaryClassObject
instance Cast NSDictionaryClassObject NSDictionaryClassObject
    where cast = id
instance Upcast NSDictionaryClassObject NSDictionaryClassObject
instance Downcast NSDictionaryClassObject NSDictionaryClassObject
instance NSObjectClassD NSDictionaryClassObject
instance Cast NSDictionaryClassObject NSObjectClassObject
    where cast (NSDictionaryClassObject p) = NSObjectClassObject (castPtr p)
instance Cast NSObjectClassObject NSDictionaryClassObject
    where cast (NSObjectClassObject p) = NSDictionaryClassObject (castPtr p)
instance Upcast NSDictionaryClassObject NSObjectClassObject
instance Downcast NSObjectClassObject NSDictionaryClassObject
newtype NSInvocationClassObject = NSInvocationClassObject (Ptr NSInvocationClassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSInvocationClassObject, Downcast NSInvocationClassObject
						c, NSObjectClassD c) => NSInvocationClassD c
instance NSInvocationClassD NSInvocationClassObject
instance Cast NSInvocationClassObject NSInvocationClassObject
    where cast = id
instance Upcast NSInvocationClassObject NSInvocationClassObject
instance Downcast NSInvocationClassObject NSInvocationClassObject
instance NSObjectClassD NSInvocationClassObject
instance Cast NSInvocationClassObject NSObjectClassObject
    where cast (NSInvocationClassObject p) = NSObjectClassObject (castPtr p)
instance Cast NSObjectClassObject NSInvocationClassObject
    where cast (NSObjectClassObject p) = NSInvocationClassObject (castPtr p)
instance Upcast NSInvocationClassObject NSObjectClassObject
instance Downcast NSObjectClassObject NSInvocationClassObject
newtype NSMethodSignatureClassObject = NSMethodSignatureClassObject (Ptr NSMethodSignatureClassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSMethodSignatureClassObject, Downcast NSMethodSignatureClassObject
						     c, NSObjectClassD c) => NSMethodSignatureClassD c
instance NSMethodSignatureClassD NSMethodSignatureClassObject
instance Cast NSMethodSignatureClassObject
	      NSMethodSignatureClassObject
    where cast = id
instance Upcast NSMethodSignatureClassObject
		NSMethodSignatureClassObject
instance Downcast NSMethodSignatureClassObject
		  NSMethodSignatureClassObject
instance NSObjectClassD NSMethodSignatureClassObject
instance Cast NSMethodSignatureClassObject NSObjectClassObject
    where cast (NSMethodSignatureClassObject p) = NSObjectClassObject (castPtr p)
instance Cast NSObjectClassObject NSMethodSignatureClassObject
    where cast (NSObjectClassObject p) = NSMethodSignatureClassObject (castPtr p)
instance Upcast NSMethodSignatureClassObject NSObjectClassObject
instance Downcast NSObjectClassObject NSMethodSignatureClassObject
newtype NSMovieClassObject = NSMovieClassObject (Ptr NSMovieClassObject)
    deriving (Eq, Show)
class (Upcast c NSMovieClassObject, Downcast NSMovieClassObject
					     c, NSObjectClassD c) => NSMovieClassD c
instance NSMovieClassD NSMovieClassObject
instance Cast NSMovieClassObject NSMovieClassObject
    where cast = id
instance Upcast NSMovieClassObject NSMovieClassObject
instance Downcast NSMovieClassObject NSMovieClassObject
instance NSObjectClassD NSMovieClassObject
instance Cast NSMovieClassObject NSObjectClassObject
    where cast (NSMovieClassObject p) = NSObjectClassObject (castPtr p)
instance Cast NSObjectClassObject NSMovieClassObject
    where cast (NSObjectClassObject p) = NSMovieClassObject (castPtr p)
instance Upcast NSMovieClassObject NSObjectClassObject
instance Downcast NSObjectClassObject NSMovieClassObject
newtype NSMovieViewClassObject = NSMovieViewClassObject (Ptr NSMovieViewClassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSMovieViewClassObject, Downcast NSMovieViewClassObject
					       c, NSViewClassD c) => NSMovieViewClassD c
instance NSMovieViewClassD NSMovieViewClassObject
instance Cast NSMovieViewClassObject NSMovieViewClassObject
    where cast = id
instance Upcast NSMovieViewClassObject NSMovieViewClassObject
instance Downcast NSMovieViewClassObject NSMovieViewClassObject
instance NSViewClassD NSMovieViewClassObject
instance Cast NSMovieViewClassObject NSViewClassObject
    where cast (NSMovieViewClassObject p) = NSViewClassObject (castPtr p)
instance Cast NSViewClassObject NSMovieViewClassObject
    where cast (NSViewClassObject p) = NSMovieViewClassObject (castPtr p)
instance Upcast NSMovieViewClassObject NSViewClassObject
instance Downcast NSViewClassObject NSMovieViewClassObject
instance NSResponderClassD NSMovieViewClassObject
instance Cast NSMovieViewClassObject NSResponderClassObject
    where cast (NSMovieViewClassObject p) = NSResponderClassObject (castPtr p)
instance Cast NSResponderClassObject NSMovieViewClassObject
    where cast (NSResponderClassObject p) = NSMovieViewClassObject (castPtr p)
instance Upcast NSMovieViewClassObject NSResponderClassObject
instance Downcast NSResponderClassObject NSMovieViewClassObject
instance NSObjectClassD NSMovieViewClassObject
instance Cast NSMovieViewClassObject NSObjectClassObject
    where cast (NSMovieViewClassObject p) = NSObjectClassObject (castPtr p)
instance Cast NSObjectClassObject NSMovieViewClassObject
    where cast (NSObjectClassObject p) = NSMovieViewClassObject (castPtr p)
instance Upcast NSMovieViewClassObject NSObjectClassObject
instance Downcast NSObjectClassObject NSMovieViewClassObject
newtype NSNumberClassObject = NSNumberClassObject (Ptr NSNumberClassObject)
    deriving (Eq, Show)
class (Upcast c NSNumberClassObject, Downcast NSNumberClassObject
					      c, NSValueClassD c) => NSNumberClassD c
instance NSNumberClassD NSNumberClassObject
instance Cast NSNumberClassObject NSNumberClassObject
    where cast = id
instance Upcast NSNumberClassObject NSNumberClassObject
instance Downcast NSNumberClassObject NSNumberClassObject
instance NSValueClassD NSNumberClassObject
instance Cast NSNumberClassObject NSValueClassObject
    where cast (NSNumberClassObject p) = NSValueClassObject (castPtr p)
instance Cast NSValueClassObject NSNumberClassObject
    where cast (NSValueClassObject p) = NSNumberClassObject (castPtr p)
instance Upcast NSNumberClassObject NSValueClassObject
instance Downcast NSValueClassObject NSNumberClassObject
instance NSObjectClassD NSNumberClassObject
instance Cast NSNumberClassObject NSObjectClassObject
    where cast (NSNumberClassObject p) = NSObjectClassObject (castPtr p)
instance Cast NSObjectClassObject NSNumberClassObject
    where cast (NSObjectClassObject p) = NSNumberClassObject (castPtr p)
instance Upcast NSNumberClassObject NSObjectClassObject
instance Downcast NSObjectClassObject NSNumberClassObject
newtype NSResponderClassObject = NSResponderClassObject (Ptr NSResponderClassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSResponderClassObject, Downcast NSResponderClassObject
					       c, NSObjectClassD c) => NSResponderClassD c
instance NSResponderClassD NSResponderClassObject
instance Cast NSResponderClassObject NSResponderClassObject
    where cast = id
instance Upcast NSResponderClassObject NSResponderClassObject
instance Downcast NSResponderClassObject NSResponderClassObject
instance NSObjectClassD NSResponderClassObject
instance Cast NSResponderClassObject NSObjectClassObject
    where cast (NSResponderClassObject p) = NSObjectClassObject (castPtr p)
instance Cast NSObjectClassObject NSResponderClassObject
    where cast (NSObjectClassObject p) = NSResponderClassObject (castPtr p)
instance Upcast NSResponderClassObject NSObjectClassObject
instance Downcast NSObjectClassObject NSResponderClassObject
newtype NSStringClassObject = NSStringClassObject (Ptr NSStringClassObject)
    deriving (Eq, Show)
class (Upcast c NSStringClassObject, Downcast NSStringClassObject
					      c, NSObjectClassD c) => NSStringClassD c
instance NSStringClassD NSStringClassObject
instance Cast NSStringClassObject NSStringClassObject
    where cast = id
instance Upcast NSStringClassObject NSStringClassObject
instance Downcast NSStringClassObject NSStringClassObject
instance NSObjectClassD NSStringClassObject
instance Cast NSStringClassObject NSObjectClassObject
    where cast (NSStringClassObject p) = NSObjectClassObject (castPtr p)
instance Cast NSObjectClassObject NSStringClassObject
    where cast (NSObjectClassObject p) = NSStringClassObject (castPtr p)
instance Upcast NSStringClassObject NSObjectClassObject
instance Downcast NSObjectClassObject NSStringClassObject
newtype NSTextClassObject = NSTextClassObject (Ptr NSTextClassObject)
    deriving (Eq, Show)
class (Upcast c NSTextClassObject, Downcast NSTextClassObject
					    c, NSViewClassD c) => NSTextClassD c
instance NSTextClassD NSTextClassObject
instance Cast NSTextClassObject NSTextClassObject
    where cast = id
instance Upcast NSTextClassObject NSTextClassObject
instance Downcast NSTextClassObject NSTextClassObject
instance NSViewClassD NSTextClassObject
instance Cast NSTextClassObject NSViewClassObject
    where cast (NSTextClassObject p) = NSViewClassObject (castPtr p)
instance Cast NSViewClassObject NSTextClassObject
    where cast (NSViewClassObject p) = NSTextClassObject (castPtr p)
instance Upcast NSTextClassObject NSViewClassObject
instance Downcast NSViewClassObject NSTextClassObject
instance NSResponderClassD NSTextClassObject
instance Cast NSTextClassObject NSResponderClassObject
    where cast (NSTextClassObject p) = NSResponderClassObject (castPtr p)
instance Cast NSResponderClassObject NSTextClassObject
    where cast (NSResponderClassObject p) = NSTextClassObject (castPtr p)
instance Upcast NSTextClassObject NSResponderClassObject
instance Downcast NSResponderClassObject NSTextClassObject
instance NSObjectClassD NSTextClassObject
instance Cast NSTextClassObject NSObjectClassObject
    where cast (NSTextClassObject p) = NSObjectClassObject (castPtr p)
instance Cast NSObjectClassObject NSTextClassObject
    where cast (NSObjectClassObject p) = NSTextClassObject (castPtr p)
instance Upcast NSTextClassObject NSObjectClassObject
instance Downcast NSObjectClassObject NSTextClassObject
newtype NSTextFieldClassObject = NSTextFieldClassObject (Ptr NSTextFieldClassObject)
    deriving (Eq, Show)
class (Upcast c
	      NSTextFieldClassObject, Downcast NSTextFieldClassObject
					       c, NSTextClassD c) => NSTextFieldClassD c
instance NSTextFieldClassD NSTextFieldClassObject
instance Cast NSTextFieldClassObject NSTextFieldClassObject
    where cast = id
instance Upcast NSTextFieldClassObject NSTextFieldClassObject
instance Downcast NSTextFieldClassObject NSTextFieldClassObject
instance NSTextClassD NSTextFieldClassObject
instance Cast NSTextFieldClassObject NSTextClassObject
    where cast (NSTextFieldClassObject p) = NSTextClassObject (castPtr p)
instance Cast NSTextClassObject NSTextFieldClassObject
    where cast (NSTextClassObject p) = NSTextFieldClassObject (castPtr p)
instance Upcast NSTextFieldClassObject NSTextClassObject
instance Downcast NSTextClassObject NSTextFieldClassObject
instance NSViewClassD NSTextFieldClassObject
instance Cast NSTextFieldClassObject NSViewClassObject
    where cast (NSTextFieldClassObject p) = NSViewClassObject (castPtr p)
instance Cast NSViewClassObject NSTextFieldClassObject
    where cast (NSViewClassObject p) = NSTextFieldClassObject (castPtr p)
instance Upcast NSTextFieldClassObject NSViewClassObject
instance Downcast NSViewClassObject NSTextFieldClassObject
instance NSResponderClassD NSTextFieldClassObject
instance Cast NSTextFieldClassObject NSResponderClassObject
    where cast (NSTextFieldClassObject p) = NSResponderClassObject (castPtr p)
instance Cast NSResponderClassObject NSTextFieldClassObject
    where cast (NSResponderClassObject p) = NSTextFieldClassObject (castPtr p)
instance Upcast NSTextFieldClassObject NSResponderClassObject
instance Downcast NSResponderClassObject NSTextFieldClassObject
instance NSObjectClassD NSTextFieldClassObject
instance Cast NSTextFieldClassObject NSObjectClassObject
    where cast (NSTextFieldClassObject p) = NSObjectClassObject (castPtr p)
instance Cast NSObjectClassObject NSTextFieldClassObject
    where cast (NSObjectClassObject p) = NSTextFieldClassObject (castPtr p)
instance Upcast NSTextFieldClassObject NSObjectClassObject
instance Downcast NSObjectClassObject NSTextFieldClassObject
newtype NSURLClassObject = NSURLClassObject (Ptr NSURLClassObject)
    deriving (Eq, Show)
class (Upcast c NSURLClassObject, Downcast NSURLClassObject
					   c, NSObjectClassD c) => NSURLClassD c
instance NSURLClassD NSURLClassObject
instance Cast NSURLClassObject NSURLClassObject
    where cast = id
instance Upcast NSURLClassObject NSURLClassObject
instance Downcast NSURLClassObject NSURLClassObject
instance NSObjectClassD NSURLClassObject
instance Cast NSURLClassObject NSObjectClassObject
    where cast (NSURLClassObject p) = NSObjectClassObject (castPtr p)
instance Cast NSObjectClassObject NSURLClassObject
    where cast (NSObjectClassObject p) = NSURLClassObject (castPtr p)
instance Upcast NSURLClassObject NSObjectClassObject
instance Downcast NSObjectClassObject NSURLClassObject
newtype NSValueClassObject = NSValueClassObject (Ptr NSValueClassObject)
    deriving (Eq, Show)
class (Upcast c NSValueClassObject, Downcast NSValueClassObject
					     c, NSObjectClassD c) => NSValueClassD c
instance NSValueClassD NSValueClassObject
instance Cast NSValueClassObject NSValueClassObject
    where cast = id
instance Upcast NSValueClassObject NSValueClassObject
instance Downcast NSValueClassObject NSValueClassObject
instance NSObjectClassD NSValueClassObject
instance Cast NSValueClassObject NSObjectClassObject
    where cast (NSValueClassObject p) = NSObjectClassObject (castPtr p)
instance Cast NSObjectClassObject NSValueClassObject
    where cast (NSObjectClassObject p) = NSValueClassObject (castPtr p)
instance Upcast NSValueClassObject NSObjectClassObject
instance Downcast NSObjectClassObject NSValueClassObject
newtype NSViewClassObject = NSViewClassObject (Ptr NSViewClassObject)
    deriving (Eq, Show)
class (Upcast c NSViewClassObject, Downcast NSViewClassObject
					    c, NSResponderClassD c) => NSViewClassD c
instance NSViewClassD NSViewClassObject
instance Cast NSViewClassObject NSViewClassObject
    where cast = id
instance Upcast NSViewClassObject NSViewClassObject
instance Downcast NSViewClassObject NSViewClassObject
instance NSResponderClassD NSViewClassObject
instance Cast NSViewClassObject NSResponderClassObject
    where cast (NSViewClassObject p) = NSResponderClassObject (castPtr p)
instance Cast NSResponderClassObject NSViewClassObject
    where cast (NSResponderClassObject p) = NSViewClassObject (castPtr p)
instance Upcast NSViewClassObject NSResponderClassObject
instance Downcast NSResponderClassObject NSViewClassObject
instance NSObjectClassD NSViewClassObject
instance Cast NSViewClassObject NSObjectClassObject
    where cast (NSViewClassObject p) = NSObjectClassObject (castPtr p)
instance Cast NSObjectClassObject NSViewClassObject
    where cast (NSObjectClassObject p) = NSViewClassObject (castPtr p)
instance Upcast NSViewClassObject NSObjectClassObject
instance Downcast NSObjectClassObject NSViewClassObject
newtype HSProxyClassObject = HSProxyClassObject (Ptr HSProxyClassObject)
    deriving (Eq, Show)
class (Upcast c HSProxyClassObject, Downcast HSProxyClassObject
					     c, NSObjectClassD c) => HSProxyClassD c
instance HSProxyClassD HSProxyClassObject
instance Cast HSProxyClassObject HSProxyClassObject
    where cast = id
instance Upcast HSProxyClassObject HSProxyClassObject
instance Downcast HSProxyClassObject HSProxyClassObject
instance NSObjectClassD HSProxyClassObject
instance Cast HSProxyClassObject NSObjectClassObject
    where cast (HSProxyClassObject p) = NSObjectClassObject (castPtr p)
instance Cast NSObjectClassObject HSProxyClassObject
    where cast (NSObjectClassObject p) = HSProxyClassObject (castPtr p)
instance Upcast HSProxyClassObject NSObjectClassObject
instance Downcast NSObjectClassObject HSProxyClassObject
newtype NSObject = NSObject (Ptr NSObject)
    deriving (Eq, Show)
class (Upcast c NSObject, Downcast NSObject c) => SubNSObject c
instance SubNSObject NSObject
instance Cast NSObject NSObject
    where cast = id
instance Upcast NSObject NSObject
instance Downcast NSObject NSObject
newtype NSArray = NSArray (Ptr NSArray)
    deriving (Eq, Show)
class (Upcast c NSArray, Downcast NSArray
				  c, SubNSObject c) => SubNSArray c
instance SubNSArray NSArray
instance Cast NSArray NSArray
    where cast = id
instance Upcast NSArray NSArray
instance Downcast NSArray NSArray
instance SubNSObject NSArray
instance Cast NSArray NSObject
    where cast (NSArray p) = NSObject (castPtr p)
instance Cast NSObject NSArray
    where cast (NSObject p) = NSArray (castPtr p)
instance Upcast NSArray NSObject
instance Downcast NSObject NSArray
newtype NSDictionary = NSDictionary (Ptr NSDictionary)
    deriving (Eq, Show)
class (Upcast c NSDictionary, Downcast NSDictionary
				       c, SubNSObject c) => SubNSDictionary c
instance SubNSDictionary NSDictionary
instance Cast NSDictionary NSDictionary
    where cast = id
instance Upcast NSDictionary NSDictionary
instance Downcast NSDictionary NSDictionary
instance SubNSObject NSDictionary
instance Cast NSDictionary NSObject
    where cast (NSDictionary p) = NSObject (castPtr p)
instance Cast NSObject NSDictionary
    where cast (NSObject p) = NSDictionary (castPtr p)
instance Upcast NSDictionary NSObject
instance Downcast NSObject NSDictionary
newtype NSInvocation = NSInvocation (Ptr NSInvocation)
    deriving (Eq, Show)
class (Upcast c NSInvocation, Downcast NSInvocation
				       c, SubNSObject c) => SubNSInvocation c
instance SubNSInvocation NSInvocation
instance Cast NSInvocation NSInvocation
    where cast = id
instance Upcast NSInvocation NSInvocation
instance Downcast NSInvocation NSInvocation
instance SubNSObject NSInvocation
instance Cast NSInvocation NSObject
    where cast (NSInvocation p) = NSObject (castPtr p)
instance Cast NSObject NSInvocation
    where cast (NSObject p) = NSInvocation (castPtr p)
instance Upcast NSInvocation NSObject
instance Downcast NSObject NSInvocation
newtype NSMethodSignature = NSMethodSignature (Ptr NSMethodSignature)
    deriving (Eq, Show)
class (Upcast c NSMethodSignature, Downcast NSMethodSignature
					    c, SubNSObject c) => SubNSMethodSignature c
instance SubNSMethodSignature NSMethodSignature
instance Cast NSMethodSignature NSMethodSignature
    where cast = id
instance Upcast NSMethodSignature NSMethodSignature
instance Downcast NSMethodSignature NSMethodSignature
instance SubNSObject NSMethodSignature
instance Cast NSMethodSignature NSObject
    where cast (NSMethodSignature p) = NSObject (castPtr p)
instance Cast NSObject NSMethodSignature
    where cast (NSObject p) = NSMethodSignature (castPtr p)
instance Upcast NSMethodSignature NSObject
instance Downcast NSObject NSMethodSignature
newtype NSMovie = NSMovie (Ptr NSMovie)
    deriving (Eq, Show)
class (Upcast c NSMovie, Downcast NSMovie
				  c, SubNSObject c) => SubNSMovie c
instance SubNSMovie NSMovie
instance Cast NSMovie NSMovie
    where cast = id
instance Upcast NSMovie NSMovie
instance Downcast NSMovie NSMovie
instance SubNSObject NSMovie
instance Cast NSMovie NSObject
    where cast (NSMovie p) = NSObject (castPtr p)
instance Cast NSObject NSMovie
    where cast (NSObject p) = NSMovie (castPtr p)
instance Upcast NSMovie NSObject
instance Downcast NSObject NSMovie
newtype NSMovieView = NSMovieView (Ptr NSMovieView)
    deriving (Eq, Show)
class (Upcast c NSMovieView, Downcast NSMovieView
				      c, SubNSView c) => SubNSMovieView c
instance SubNSMovieView NSMovieView
instance Cast NSMovieView NSMovieView
    where cast = id
instance Upcast NSMovieView NSMovieView
instance Downcast NSMovieView NSMovieView
instance SubNSView NSMovieView
instance Cast NSMovieView NSView
    where cast (NSMovieView p) = NSView (castPtr p)
instance Cast NSView NSMovieView
    where cast (NSView p) = NSMovieView (castPtr p)
instance Upcast NSMovieView NSView
instance Downcast NSView NSMovieView
instance SubNSResponder NSMovieView
instance Cast NSMovieView NSResponder
    where cast (NSMovieView p) = NSResponder (castPtr p)
instance Cast NSResponder NSMovieView
    where cast (NSResponder p) = NSMovieView (castPtr p)
instance Upcast NSMovieView NSResponder
instance Downcast NSResponder NSMovieView
instance SubNSObject NSMovieView
instance Cast NSMovieView NSObject
    where cast (NSMovieView p) = NSObject (castPtr p)
instance Cast NSObject NSMovieView
    where cast (NSObject p) = NSMovieView (castPtr p)
instance Upcast NSMovieView NSObject
instance Downcast NSObject NSMovieView
newtype NSNumber = NSNumber (Ptr NSNumber)
    deriving (Eq, Show)
class (Upcast c NSNumber, Downcast NSNumber
				   c, SubNSValue c) => SubNSNumber c
instance SubNSNumber NSNumber
instance Cast NSNumber NSNumber
    where cast = id
instance Upcast NSNumber NSNumber
instance Downcast NSNumber NSNumber
instance SubNSValue NSNumber
instance Cast NSNumber NSValue
    where cast (NSNumber p) = NSValue (castPtr p)
instance Cast NSValue NSNumber
    where cast (NSValue p) = NSNumber (castPtr p)
instance Upcast NSNumber NSValue
instance Downcast NSValue NSNumber
instance SubNSObject NSNumber
instance Cast NSNumber NSObject
    where cast (NSNumber p) = NSObject (castPtr p)
instance Cast NSObject NSNumber
    where cast (NSObject p) = NSNumber (castPtr p)
instance Upcast NSNumber NSObject
instance Downcast NSObject NSNumber
newtype NSResponder = NSResponder (Ptr NSResponder)
    deriving (Eq, Show)
class (Upcast c NSResponder, Downcast NSResponder
				      c, SubNSObject c) => SubNSResponder c
instance SubNSResponder NSResponder
instance Cast NSResponder NSResponder
    where cast = id
instance Upcast NSResponder NSResponder
instance Downcast NSResponder NSResponder
instance SubNSObject NSResponder
instance Cast NSResponder NSObject
    where cast (NSResponder p) = NSObject (castPtr p)
instance Cast NSObject NSResponder
    where cast (NSObject p) = NSResponder (castPtr p)
instance Upcast NSResponder NSObject
instance Downcast NSObject NSResponder
newtype NSString = NSString (Ptr NSString)
    deriving (Eq, Show)
class (Upcast c NSString, Downcast NSString
				   c, SubNSObject c) => SubNSString c
instance SubNSString NSString
instance Cast NSString NSString
    where cast = id
instance Upcast NSString NSString
instance Downcast NSString NSString
instance SubNSObject NSString
instance Cast NSString NSObject
    where cast (NSString p) = NSObject (castPtr p)
instance Cast NSObject NSString
    where cast (NSObject p) = NSString (castPtr p)
instance Upcast NSString NSObject
instance Downcast NSObject NSString
newtype NSText = NSText (Ptr NSText)
    deriving (Eq, Show)
class (Upcast c NSText, Downcast NSText
				 c, SubNSView c) => SubNSText c
instance SubNSText NSText
instance Cast NSText NSText
    where cast = id
instance Upcast NSText NSText
instance Downcast NSText NSText
instance SubNSView NSText
instance Cast NSText NSView
    where cast (NSText p) = NSView (castPtr p)
instance Cast NSView NSText
    where cast (NSView p) = NSText (castPtr p)
instance Upcast NSText NSView
instance Downcast NSView NSText
instance SubNSResponder NSText
instance Cast NSText NSResponder
    where cast (NSText p) = NSResponder (castPtr p)
instance Cast NSResponder NSText
    where cast (NSResponder p) = NSText (castPtr p)
instance Upcast NSText NSResponder
instance Downcast NSResponder NSText
instance SubNSObject NSText
instance Cast NSText NSObject
    where cast (NSText p) = NSObject (castPtr p)
instance Cast NSObject NSText
    where cast (NSObject p) = NSText (castPtr p)
instance Upcast NSText NSObject
instance Downcast NSObject NSText
newtype NSTextField = NSTextField (Ptr NSTextField)
    deriving (Eq, Show)
class (Upcast c NSTextField, Downcast NSTextField
				      c, SubNSText c) => SubNSTextField c
instance SubNSTextField NSTextField
instance Cast NSTextField NSTextField
    where cast = id
instance Upcast NSTextField NSTextField
instance Downcast NSTextField NSTextField
instance SubNSText NSTextField
instance Cast NSTextField NSText
    where cast (NSTextField p) = NSText (castPtr p)
instance Cast NSText NSTextField
    where cast (NSText p) = NSTextField (castPtr p)
instance Upcast NSTextField NSText
instance Downcast NSText NSTextField
instance SubNSView NSTextField
instance Cast NSTextField NSView
    where cast (NSTextField p) = NSView (castPtr p)
instance Cast NSView NSTextField
    where cast (NSView p) = NSTextField (castPtr p)
instance Upcast NSTextField NSView
instance Downcast NSView NSTextField
instance SubNSResponder NSTextField
instance Cast NSTextField NSResponder
    where cast (NSTextField p) = NSResponder (castPtr p)
instance Cast NSResponder NSTextField
    where cast (NSResponder p) = NSTextField (castPtr p)
instance Upcast NSTextField NSResponder
instance Downcast NSResponder NSTextField
instance SubNSObject NSTextField
instance Cast NSTextField NSObject
    where cast (NSTextField p) = NSObject (castPtr p)
instance Cast NSObject NSTextField
    where cast (NSObject p) = NSTextField (castPtr p)
instance Upcast NSTextField NSObject
instance Downcast NSObject NSTextField
newtype NSURL = NSURL (Ptr NSURL)
    deriving (Eq, Show)
class (Upcast c NSURL, Downcast NSURL
				c, SubNSObject c) => SubNSURL c
instance SubNSURL NSURL
instance Cast NSURL NSURL
    where cast = id
instance Upcast NSURL NSURL
instance Downcast NSURL NSURL
instance SubNSObject NSURL
instance Cast NSURL NSObject
    where cast (NSURL p) = NSObject (castPtr p)
instance Cast NSObject NSURL
    where cast (NSObject p) = NSURL (castPtr p)
instance Upcast NSURL NSObject
instance Downcast NSObject NSURL
newtype NSValue = NSValue (Ptr NSValue)
    deriving (Eq, Show)
class (Upcast c NSValue, Downcast NSValue
				  c, SubNSObject c) => SubNSValue c
instance SubNSValue NSValue
instance Cast NSValue NSValue
    where cast = id
instance Upcast NSValue NSValue
instance Downcast NSValue NSValue
instance SubNSObject NSValue
instance Cast NSValue NSObject
    where cast (NSValue p) = NSObject (castPtr p)
instance Cast NSObject NSValue
    where cast (NSObject p) = NSValue (castPtr p)
instance Upcast NSValue NSObject
instance Downcast NSObject NSValue
newtype NSView = NSView (Ptr NSView)
    deriving (Eq, Show)
class (Upcast c NSView, Downcast NSView
				 c, SubNSResponder c) => SubNSView c
instance SubNSView NSView
instance Cast NSView NSView
    where cast = id
instance Upcast NSView NSView
instance Downcast NSView NSView
instance SubNSResponder NSView
instance Cast NSView NSResponder
    where cast (NSView p) = NSResponder (castPtr p)
instance Cast NSResponder NSView
    where cast (NSResponder p) = NSView (castPtr p)
instance Upcast NSView NSResponder
instance Downcast NSResponder NSView
instance SubNSObject NSView
instance Cast NSView NSObject
    where cast (NSView p) = NSObject (castPtr p)
instance Cast NSObject NSView
    where cast (NSObject p) = NSView (castPtr p)
instance Upcast NSView NSObject
instance Downcast NSObject NSView
newtype HSProxy = HSProxy (Ptr HSProxy)
    deriving (Eq, Show)
class (Upcast c HSProxy, Downcast HSProxy
				  c, SubNSObject c) => SubHSProxy c
instance SubHSProxy HSProxy
instance Cast HSProxy HSProxy
    where cast = id
instance Upcast HSProxy HSProxy
instance Downcast HSProxy HSProxy
instance SubNSObject HSProxy
instance Cast HSProxy NSObject
    where cast (HSProxy p) = NSObject (castPtr p)
instance Cast NSObject HSProxy
    where cast (NSObject p) = HSProxy (castPtr p)
instance Upcast HSProxy NSObject
instance Downcast NSObject HSProxy
instance ObjCObject NSObjectMetaclassObject
instance Cast NSObjectMetaclassObject ID
    where cast (NSObjectMetaclassObject p) = ID (castPtr p)
instance Cast ID NSObjectMetaclassObject
    where cast (ID p) = NSObjectMetaclassObject (castPtr p)
instance Upcast NSObjectMetaclassObject ID
instance Downcast ID NSObjectMetaclassObject
instance ObjCObject NSArrayMetaclassObject
instance Cast NSArrayMetaclassObject ID
    where cast (NSArrayMetaclassObject p) = ID (castPtr p)
instance Cast ID NSArrayMetaclassObject
    where cast (ID p) = NSArrayMetaclassObject (castPtr p)
instance Upcast NSArrayMetaclassObject ID
instance Downcast ID NSArrayMetaclassObject
instance ObjCObject NSDictionaryMetaclassObject
instance Cast NSDictionaryMetaclassObject ID
    where cast (NSDictionaryMetaclassObject p) = ID (castPtr p)
instance Cast ID NSDictionaryMetaclassObject
    where cast (ID p) = NSDictionaryMetaclassObject (castPtr p)
instance Upcast NSDictionaryMetaclassObject ID
instance Downcast ID NSDictionaryMetaclassObject
instance ObjCObject NSInvocationMetaclassObject
instance Cast NSInvocationMetaclassObject ID
    where cast (NSInvocationMetaclassObject p) = ID (castPtr p)
instance Cast ID NSInvocationMetaclassObject
    where cast (ID p) = NSInvocationMetaclassObject (castPtr p)
instance Upcast NSInvocationMetaclassObject ID
instance Downcast ID NSInvocationMetaclassObject
instance ObjCObject NSMethodSignatureMetaclassObject
instance Cast NSMethodSignatureMetaclassObject ID
    where cast (NSMethodSignatureMetaclassObject p) = ID (castPtr p)
instance Cast ID NSMethodSignatureMetaclassObject
    where cast (ID p) = NSMethodSignatureMetaclassObject (castPtr p)
instance Upcast NSMethodSignatureMetaclassObject ID
instance Downcast ID NSMethodSignatureMetaclassObject
instance ObjCObject NSMovieMetaclassObject
instance Cast NSMovieMetaclassObject ID
    where cast (NSMovieMetaclassObject p) = ID (castPtr p)
instance Cast ID NSMovieMetaclassObject
    where cast (ID p) = NSMovieMetaclassObject (castPtr p)
instance Upcast NSMovieMetaclassObject ID
instance Downcast ID NSMovieMetaclassObject
instance ObjCObject NSMovieViewMetaclassObject
instance Cast NSMovieViewMetaclassObject ID
    where cast (NSMovieViewMetaclassObject p) = ID (castPtr p)
instance Cast ID NSMovieViewMetaclassObject
    where cast (ID p) = NSMovieViewMetaclassObject (castPtr p)
instance Upcast NSMovieViewMetaclassObject ID
instance Downcast ID NSMovieViewMetaclassObject
instance ObjCObject NSNumberMetaclassObject
instance Cast NSNumberMetaclassObject ID
    where cast (NSNumberMetaclassObject p) = ID (castPtr p)
instance Cast ID NSNumberMetaclassObject
    where cast (ID p) = NSNumberMetaclassObject (castPtr p)
instance Upcast NSNumberMetaclassObject ID
instance Downcast ID NSNumberMetaclassObject
instance ObjCObject NSResponderMetaclassObject
instance Cast NSResponderMetaclassObject ID
    where cast (NSResponderMetaclassObject p) = ID (castPtr p)
instance Cast ID NSResponderMetaclassObject
    where cast (ID p) = NSResponderMetaclassObject (castPtr p)
instance Upcast NSResponderMetaclassObject ID
instance Downcast ID NSResponderMetaclassObject
instance ObjCObject NSStringMetaclassObject
instance Cast NSStringMetaclassObject ID
    where cast (NSStringMetaclassObject p) = ID (castPtr p)
instance Cast ID NSStringMetaclassObject
    where cast (ID p) = NSStringMetaclassObject (castPtr p)
instance Upcast NSStringMetaclassObject ID
instance Downcast ID NSStringMetaclassObject
instance ObjCObject NSTextMetaclassObject
instance Cast NSTextMetaclassObject ID
    where cast (NSTextMetaclassObject p) = ID (castPtr p)
instance Cast ID NSTextMetaclassObject
    where cast (ID p) = NSTextMetaclassObject (castPtr p)
instance Upcast NSTextMetaclassObject ID
instance Downcast ID NSTextMetaclassObject
instance ObjCObject NSTextFieldMetaclassObject
instance Cast NSTextFieldMetaclassObject ID
    where cast (NSTextFieldMetaclassObject p) = ID (castPtr p)
instance Cast ID NSTextFieldMetaclassObject
    where cast (ID p) = NSTextFieldMetaclassObject (castPtr p)
instance Upcast NSTextFieldMetaclassObject ID
instance Downcast ID NSTextFieldMetaclassObject
instance ObjCObject NSURLMetaclassObject
instance Cast NSURLMetaclassObject ID
    where cast (NSURLMetaclassObject p) = ID (castPtr p)
instance Cast ID NSURLMetaclassObject
    where cast (ID p) = NSURLMetaclassObject (castPtr p)
instance Upcast NSURLMetaclassObject ID
instance Downcast ID NSURLMetaclassObject
instance ObjCObject NSValueMetaclassObject
instance Cast NSValueMetaclassObject ID
    where cast (NSValueMetaclassObject p) = ID (castPtr p)
instance Cast ID NSValueMetaclassObject
    where cast (ID p) = NSValueMetaclassObject (castPtr p)
instance Upcast NSValueMetaclassObject ID
instance Downcast ID NSValueMetaclassObject
instance ObjCObject NSViewMetaclassObject
instance Cast NSViewMetaclassObject ID
    where cast (NSViewMetaclassObject p) = ID (castPtr p)
instance Cast ID NSViewMetaclassObject
    where cast (ID p) = NSViewMetaclassObject (castPtr p)
instance Upcast NSViewMetaclassObject ID
instance Downcast ID NSViewMetaclassObject
instance ObjCObject HSProxyMetaclassObject
instance Cast HSProxyMetaclassObject ID
    where cast (HSProxyMetaclassObject p) = ID (castPtr p)
instance Cast ID HSProxyMetaclassObject
    where cast (ID p) = HSProxyMetaclassObject (castPtr p)
instance Upcast HSProxyMetaclassObject ID
instance Downcast ID HSProxyMetaclassObject
instance ObjCMetaclass NSObjectMetaclassObject
    where metaclassObjectName _ = "NSObject"
instance Cast NSObjectMetaclassObject ObjCMetaclassObject
    where cast (NSObjectMetaclassObject p) = ObjCMetaclassObject (castPtr p)
instance Cast ObjCMetaclassObject NSObjectMetaclassObject
    where cast (ObjCMetaclassObject p) = NSObjectMetaclassObject (castPtr p)
instance Upcast NSObjectMetaclassObject ObjCMetaclassObject
instance Downcast ObjCMetaclassObject NSObjectMetaclassObject
instance ObjCMetaclass NSArrayMetaclassObject
    where metaclassObjectName _ = "NSArray"
instance Cast NSArrayMetaclassObject ObjCMetaclassObject
    where cast (NSArrayMetaclassObject p) = ObjCMetaclassObject (castPtr p)
instance Cast ObjCMetaclassObject NSArrayMetaclassObject
    where cast (ObjCMetaclassObject p) = NSArrayMetaclassObject (castPtr p)
instance Upcast NSArrayMetaclassObject ObjCMetaclassObject
instance Downcast ObjCMetaclassObject NSArrayMetaclassObject
instance ObjCMetaclass NSDictionaryMetaclassObject
    where metaclassObjectName _ = "NSDictionary"
instance Cast NSDictionaryMetaclassObject ObjCMetaclassObject
    where cast (NSDictionaryMetaclassObject p) = ObjCMetaclassObject (castPtr p)
instance Cast ObjCMetaclassObject NSDictionaryMetaclassObject
    where cast (ObjCMetaclassObject p) = NSDictionaryMetaclassObject (castPtr p)
instance Upcast NSDictionaryMetaclassObject ObjCMetaclassObject
instance Downcast ObjCMetaclassObject NSDictionaryMetaclassObject
instance ObjCMetaclass NSInvocationMetaclassObject
    where metaclassObjectName _ = "NSInvocation"
instance Cast NSInvocationMetaclassObject ObjCMetaclassObject
    where cast (NSInvocationMetaclassObject p) = ObjCMetaclassObject (castPtr p)
instance Cast ObjCMetaclassObject NSInvocationMetaclassObject
    where cast (ObjCMetaclassObject p) = NSInvocationMetaclassObject (castPtr p)
instance Upcast NSInvocationMetaclassObject ObjCMetaclassObject
instance Downcast ObjCMetaclassObject NSInvocationMetaclassObject
instance ObjCMetaclass NSMethodSignatureMetaclassObject
    where metaclassObjectName _ = "NSMethodSignature"
instance Cast NSMethodSignatureMetaclassObject ObjCMetaclassObject
    where cast (NSMethodSignatureMetaclassObject p) = ObjCMetaclassObject (castPtr p)
instance Cast ObjCMetaclassObject NSMethodSignatureMetaclassObject
    where cast (ObjCMetaclassObject p) = NSMethodSignatureMetaclassObject (castPtr p)
instance Upcast NSMethodSignatureMetaclassObject
		ObjCMetaclassObject
instance Downcast ObjCMetaclassObject
		  NSMethodSignatureMetaclassObject
instance ObjCMetaclass NSMovieMetaclassObject
    where metaclassObjectName _ = "NSMovie"
instance Cast NSMovieMetaclassObject ObjCMetaclassObject
    where cast (NSMovieMetaclassObject p) = ObjCMetaclassObject (castPtr p)
instance Cast ObjCMetaclassObject NSMovieMetaclassObject
    where cast (ObjCMetaclassObject p) = NSMovieMetaclassObject (castPtr p)
instance Upcast NSMovieMetaclassObject ObjCMetaclassObject
instance Downcast ObjCMetaclassObject NSMovieMetaclassObject
instance ObjCMetaclass NSMovieViewMetaclassObject
    where metaclassObjectName _ = "NSMovieView"
instance Cast NSMovieViewMetaclassObject ObjCMetaclassObject
    where cast (NSMovieViewMetaclassObject p) = ObjCMetaclassObject (castPtr p)
instance Cast ObjCMetaclassObject NSMovieViewMetaclassObject
    where cast (ObjCMetaclassObject p) = NSMovieViewMetaclassObject (castPtr p)
instance Upcast NSMovieViewMetaclassObject ObjCMetaclassObject
instance Downcast ObjCMetaclassObject NSMovieViewMetaclassObject
instance ObjCMetaclass NSNumberMetaclassObject
    where metaclassObjectName _ = "NSNumber"
instance Cast NSNumberMetaclassObject ObjCMetaclassObject
    where cast (NSNumberMetaclassObject p) = ObjCMetaclassObject (castPtr p)
instance Cast ObjCMetaclassObject NSNumberMetaclassObject
    where cast (ObjCMetaclassObject p) = NSNumberMetaclassObject (castPtr p)
instance Upcast NSNumberMetaclassObject ObjCMetaclassObject
instance Downcast ObjCMetaclassObject NSNumberMetaclassObject
instance ObjCMetaclass NSResponderMetaclassObject
    where metaclassObjectName _ = "NSResponder"
instance Cast NSResponderMetaclassObject ObjCMetaclassObject
    where cast (NSResponderMetaclassObject p) = ObjCMetaclassObject (castPtr p)
instance Cast ObjCMetaclassObject NSResponderMetaclassObject
    where cast (ObjCMetaclassObject p) = NSResponderMetaclassObject (castPtr p)
instance Upcast NSResponderMetaclassObject ObjCMetaclassObject
instance Downcast ObjCMetaclassObject NSResponderMetaclassObject
instance ObjCMetaclass NSStringMetaclassObject
    where metaclassObjectName _ = "NSString"
instance Cast NSStringMetaclassObject ObjCMetaclassObject
    where cast (NSStringMetaclassObject p) = ObjCMetaclassObject (castPtr p)
instance Cast ObjCMetaclassObject NSStringMetaclassObject
    where cast (ObjCMetaclassObject p) = NSStringMetaclassObject (castPtr p)
instance Upcast NSStringMetaclassObject ObjCMetaclassObject
instance Downcast ObjCMetaclassObject NSStringMetaclassObject
instance ObjCMetaclass NSTextMetaclassObject
    where metaclassObjectName _ = "NSText"
instance Cast NSTextMetaclassObject ObjCMetaclassObject
    where cast (NSTextMetaclassObject p) = ObjCMetaclassObject (castPtr p)
instance Cast ObjCMetaclassObject NSTextMetaclassObject
    where cast (ObjCMetaclassObject p) = NSTextMetaclassObject (castPtr p)
instance Upcast NSTextMetaclassObject ObjCMetaclassObject
instance Downcast ObjCMetaclassObject NSTextMetaclassObject
instance ObjCMetaclass NSTextFieldMetaclassObject
    where metaclassObjectName _ = "NSTextField"
instance Cast NSTextFieldMetaclassObject ObjCMetaclassObject
    where cast (NSTextFieldMetaclassObject p) = ObjCMetaclassObject (castPtr p)
instance Cast ObjCMetaclassObject NSTextFieldMetaclassObject
    where cast (ObjCMetaclassObject p) = NSTextFieldMetaclassObject (castPtr p)
instance Upcast NSTextFieldMetaclassObject ObjCMetaclassObject
instance Downcast ObjCMetaclassObject NSTextFieldMetaclassObject
instance ObjCMetaclass NSURLMetaclassObject
    where metaclassObjectName _ = "NSURL"
instance Cast NSURLMetaclassObject ObjCMetaclassObject
    where cast (NSURLMetaclassObject p) = ObjCMetaclassObject (castPtr p)
instance Cast ObjCMetaclassObject NSURLMetaclassObject
    where cast (ObjCMetaclassObject p) = NSURLMetaclassObject (castPtr p)
instance Upcast NSURLMetaclassObject ObjCMetaclassObject
instance Downcast ObjCMetaclassObject NSURLMetaclassObject
instance ObjCMetaclass NSValueMetaclassObject
    where metaclassObjectName _ = "NSValue"
instance Cast NSValueMetaclassObject ObjCMetaclassObject
    where cast (NSValueMetaclassObject p) = ObjCMetaclassObject (castPtr p)
instance Cast ObjCMetaclassObject NSValueMetaclassObject
    where cast (ObjCMetaclassObject p) = NSValueMetaclassObject (castPtr p)
instance Upcast NSValueMetaclassObject ObjCMetaclassObject
instance Downcast ObjCMetaclassObject NSValueMetaclassObject
instance ObjCMetaclass NSViewMetaclassObject
    where metaclassObjectName _ = "NSView"
instance Cast NSViewMetaclassObject ObjCMetaclassObject
    where cast (NSViewMetaclassObject p) = ObjCMetaclassObject (castPtr p)
instance Cast ObjCMetaclassObject NSViewMetaclassObject
    where cast (ObjCMetaclassObject p) = NSViewMetaclassObject (castPtr p)
instance Upcast NSViewMetaclassObject ObjCMetaclassObject
instance Downcast ObjCMetaclassObject NSViewMetaclassObject
instance ObjCMetaclass HSProxyMetaclassObject
    where metaclassObjectName _ = "HSProxy"
instance Cast HSProxyMetaclassObject ObjCMetaclassObject
    where cast (HSProxyMetaclassObject p) = ObjCMetaclassObject (castPtr p)
instance Cast ObjCMetaclassObject HSProxyMetaclassObject
    where cast (ObjCMetaclassObject p) = HSProxyMetaclassObject (castPtr p)
instance Upcast HSProxyMetaclassObject ObjCMetaclassObject
instance Downcast ObjCMetaclassObject HSProxyMetaclassObject
instance ObjCTypeEncoding NSObjectMetaclassObject
instance ObjCTypeEncoding NSArrayMetaclassObject
instance ObjCTypeEncoding NSDictionaryMetaclassObject
instance ObjCTypeEncoding NSInvocationMetaclassObject
instance ObjCTypeEncoding NSMethodSignatureMetaclassObject
instance ObjCTypeEncoding NSMovieMetaclassObject
instance ObjCTypeEncoding NSMovieViewMetaclassObject
instance ObjCTypeEncoding NSNumberMetaclassObject
instance ObjCTypeEncoding NSResponderMetaclassObject
instance ObjCTypeEncoding NSStringMetaclassObject
instance ObjCTypeEncoding NSTextMetaclassObject
instance ObjCTypeEncoding NSTextFieldMetaclassObject
instance ObjCTypeEncoding NSURLMetaclassObject
instance ObjCTypeEncoding NSValueMetaclassObject
instance ObjCTypeEncoding NSViewMetaclassObject
instance ObjCTypeEncoding HSProxyMetaclassObject
instance ObjCArgument NSObjectMetaclassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSArrayMetaclassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSDictionaryMetaclassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSInvocationMetaclassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSMethodSignatureMetaclassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSMovieMetaclassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSMovieViewMetaclassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSNumberMetaclassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSResponderMetaclassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSStringMetaclassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSTextMetaclassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSTextFieldMetaclassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSURLMetaclassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSValueMetaclassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSViewMetaclassObject
    where setArgument = setObjectArgument
instance ObjCArgument HSProxyMetaclassObject
    where setArgument = setObjectArgument
instance ObjCObjectArgument NSObjectMetaclassObject
instance ObjCObjectArgument NSArrayMetaclassObject
instance ObjCObjectArgument NSDictionaryMetaclassObject
instance ObjCObjectArgument NSInvocationMetaclassObject
instance ObjCObjectArgument NSMethodSignatureMetaclassObject
instance ObjCObjectArgument NSMovieMetaclassObject
instance ObjCObjectArgument NSMovieViewMetaclassObject
instance ObjCObjectArgument NSNumberMetaclassObject
instance ObjCObjectArgument NSResponderMetaclassObject
instance ObjCObjectArgument NSStringMetaclassObject
instance ObjCObjectArgument NSTextMetaclassObject
instance ObjCObjectArgument NSTextFieldMetaclassObject
instance ObjCObjectArgument NSURLMetaclassObject
instance ObjCObjectArgument NSValueMetaclassObject
instance ObjCObjectArgument NSViewMetaclassObject
instance ObjCObjectArgument HSProxyMetaclassObject
instance ObjCArguments NSObjectMetaclassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSArrayMetaclassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSDictionaryMetaclassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSInvocationMetaclassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSMethodSignatureMetaclassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSMovieMetaclassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSMovieViewMetaclassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSNumberMetaclassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSResponderMetaclassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSStringMetaclassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSTextMetaclassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSTextFieldMetaclassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSURLMetaclassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSValueMetaclassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSViewMetaclassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments HSProxyMetaclassObject
    where setArguments = setObjectArgument 1
instance ObjCMessageReply NSObjectMetaclassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSArrayMetaclassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSDictionaryMetaclassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSInvocationMetaclassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSMethodSignatureMetaclassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSMovieMetaclassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSMovieViewMetaclassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSNumberMetaclassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSResponderMetaclassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSStringMetaclassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSTextMetaclassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSTextFieldMetaclassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSURLMetaclassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSValueMetaclassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSViewMetaclassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply HSProxyMetaclassObject
    where sendMessage = sendObjectMessage
instance ObjCObjectMessageReply NSObjectMetaclassObject
instance ObjCObjectMessageReply NSArrayMetaclassObject
instance ObjCObjectMessageReply NSDictionaryMetaclassObject
instance ObjCObjectMessageReply NSInvocationMetaclassObject
instance ObjCObjectMessageReply NSMethodSignatureMetaclassObject
instance ObjCObjectMessageReply NSMovieMetaclassObject
instance ObjCObjectMessageReply NSMovieViewMetaclassObject
instance ObjCObjectMessageReply NSNumberMetaclassObject
instance ObjCObjectMessageReply NSResponderMetaclassObject
instance ObjCObjectMessageReply NSStringMetaclassObject
instance ObjCObjectMessageReply NSTextMetaclassObject
instance ObjCObjectMessageReply NSTextFieldMetaclassObject
instance ObjCObjectMessageReply NSURLMetaclassObject
instance ObjCObjectMessageReply NSValueMetaclassObject
instance ObjCObjectMessageReply NSViewMetaclassObject
instance ObjCObjectMessageReply HSProxyMetaclassObject
instance ObjCObject NSObjectClassObject
instance Cast NSObjectClassObject ID
    where cast (NSObjectClassObject p) = ID (castPtr p)
instance Cast ID NSObjectClassObject
    where cast (ID p) = NSObjectClassObject (castPtr p)
instance Upcast NSObjectClassObject ID
instance Downcast ID NSObjectClassObject
instance ObjCObject NSArrayClassObject
instance Cast NSArrayClassObject ID
    where cast (NSArrayClassObject p) = ID (castPtr p)
instance Cast ID NSArrayClassObject
    where cast (ID p) = NSArrayClassObject (castPtr p)
instance Upcast NSArrayClassObject ID
instance Downcast ID NSArrayClassObject
instance ObjCObject NSDictionaryClassObject
instance Cast NSDictionaryClassObject ID
    where cast (NSDictionaryClassObject p) = ID (castPtr p)
instance Cast ID NSDictionaryClassObject
    where cast (ID p) = NSDictionaryClassObject (castPtr p)
instance Upcast NSDictionaryClassObject ID
instance Downcast ID NSDictionaryClassObject
instance ObjCObject NSInvocationClassObject
instance Cast NSInvocationClassObject ID
    where cast (NSInvocationClassObject p) = ID (castPtr p)
instance Cast ID NSInvocationClassObject
    where cast (ID p) = NSInvocationClassObject (castPtr p)
instance Upcast NSInvocationClassObject ID
instance Downcast ID NSInvocationClassObject
instance ObjCObject NSMethodSignatureClassObject
instance Cast NSMethodSignatureClassObject ID
    where cast (NSMethodSignatureClassObject p) = ID (castPtr p)
instance Cast ID NSMethodSignatureClassObject
    where cast (ID p) = NSMethodSignatureClassObject (castPtr p)
instance Upcast NSMethodSignatureClassObject ID
instance Downcast ID NSMethodSignatureClassObject
instance ObjCObject NSMovieClassObject
instance Cast NSMovieClassObject ID
    where cast (NSMovieClassObject p) = ID (castPtr p)
instance Cast ID NSMovieClassObject
    where cast (ID p) = NSMovieClassObject (castPtr p)
instance Upcast NSMovieClassObject ID
instance Downcast ID NSMovieClassObject
instance ObjCObject NSMovieViewClassObject
instance Cast NSMovieViewClassObject ID
    where cast (NSMovieViewClassObject p) = ID (castPtr p)
instance Cast ID NSMovieViewClassObject
    where cast (ID p) = NSMovieViewClassObject (castPtr p)
instance Upcast NSMovieViewClassObject ID
instance Downcast ID NSMovieViewClassObject
instance ObjCObject NSNumberClassObject
instance Cast NSNumberClassObject ID
    where cast (NSNumberClassObject p) = ID (castPtr p)
instance Cast ID NSNumberClassObject
    where cast (ID p) = NSNumberClassObject (castPtr p)
instance Upcast NSNumberClassObject ID
instance Downcast ID NSNumberClassObject
instance ObjCObject NSResponderClassObject
instance Cast NSResponderClassObject ID
    where cast (NSResponderClassObject p) = ID (castPtr p)
instance Cast ID NSResponderClassObject
    where cast (ID p) = NSResponderClassObject (castPtr p)
instance Upcast NSResponderClassObject ID
instance Downcast ID NSResponderClassObject
instance ObjCObject NSStringClassObject
instance Cast NSStringClassObject ID
    where cast (NSStringClassObject p) = ID (castPtr p)
instance Cast ID NSStringClassObject
    where cast (ID p) = NSStringClassObject (castPtr p)
instance Upcast NSStringClassObject ID
instance Downcast ID NSStringClassObject
instance ObjCObject NSTextClassObject
instance Cast NSTextClassObject ID
    where cast (NSTextClassObject p) = ID (castPtr p)
instance Cast ID NSTextClassObject
    where cast (ID p) = NSTextClassObject (castPtr p)
instance Upcast NSTextClassObject ID
instance Downcast ID NSTextClassObject
instance ObjCObject NSTextFieldClassObject
instance Cast NSTextFieldClassObject ID
    where cast (NSTextFieldClassObject p) = ID (castPtr p)
instance Cast ID NSTextFieldClassObject
    where cast (ID p) = NSTextFieldClassObject (castPtr p)
instance Upcast NSTextFieldClassObject ID
instance Downcast ID NSTextFieldClassObject
instance ObjCObject NSURLClassObject
instance Cast NSURLClassObject ID
    where cast (NSURLClassObject p) = ID (castPtr p)
instance Cast ID NSURLClassObject
    where cast (ID p) = NSURLClassObject (castPtr p)
instance Upcast NSURLClassObject ID
instance Downcast ID NSURLClassObject
instance ObjCObject NSValueClassObject
instance Cast NSValueClassObject ID
    where cast (NSValueClassObject p) = ID (castPtr p)
instance Cast ID NSValueClassObject
    where cast (ID p) = NSValueClassObject (castPtr p)
instance Upcast NSValueClassObject ID
instance Downcast ID NSValueClassObject
instance ObjCObject NSViewClassObject
instance Cast NSViewClassObject ID
    where cast (NSViewClassObject p) = ID (castPtr p)
instance Cast ID NSViewClassObject
    where cast (ID p) = NSViewClassObject (castPtr p)
instance Upcast NSViewClassObject ID
instance Downcast ID NSViewClassObject
instance ObjCObject HSProxyClassObject
instance Cast HSProxyClassObject ID
    where cast (HSProxyClassObject p) = ID (castPtr p)
instance Cast ID HSProxyClassObject
    where cast (ID p) = HSProxyClassObject (castPtr p)
instance Upcast HSProxyClassObject ID
instance Downcast ID HSProxyClassObject
_NSObject_ :: NSObjectClassObject
_NSObject_ = getClassObject
instance ObjCClass NSObjectClassObject
    where classObjectName _ = "NSObject"
instance Cast NSObjectClassObject ObjCClassObject
    where cast (NSObjectClassObject p) = ObjCClassObject (castPtr p)
instance Cast ObjCClassObject NSObjectClassObject
    where cast (ObjCClassObject p) = NSObjectClassObject (castPtr p)
instance Upcast NSObjectClassObject ObjCClassObject
instance Downcast ObjCClassObject NSObjectClassObject
_NSArray_ :: NSArrayClassObject
_NSArray_ = getClassObject
instance ObjCClass NSArrayClassObject
    where classObjectName _ = "NSArray"
instance Cast NSArrayClassObject ObjCClassObject
    where cast (NSArrayClassObject p) = ObjCClassObject (castPtr p)
instance Cast ObjCClassObject NSArrayClassObject
    where cast (ObjCClassObject p) = NSArrayClassObject (castPtr p)
instance Upcast NSArrayClassObject ObjCClassObject
instance Downcast ObjCClassObject NSArrayClassObject
_NSDictionary_ :: NSDictionaryClassObject
_NSDictionary_ = getClassObject
instance ObjCClass NSDictionaryClassObject
    where classObjectName _ = "NSDictionary"
instance Cast NSDictionaryClassObject ObjCClassObject
    where cast (NSDictionaryClassObject p) = ObjCClassObject (castPtr p)
instance Cast ObjCClassObject NSDictionaryClassObject
    where cast (ObjCClassObject p) = NSDictionaryClassObject (castPtr p)
instance Upcast NSDictionaryClassObject ObjCClassObject
instance Downcast ObjCClassObject NSDictionaryClassObject
_NSInvocation_ :: NSInvocationClassObject
_NSInvocation_ = getClassObject
instance ObjCClass NSInvocationClassObject
    where classObjectName _ = "NSInvocation"
instance Cast NSInvocationClassObject ObjCClassObject
    where cast (NSInvocationClassObject p) = ObjCClassObject (castPtr p)
instance Cast ObjCClassObject NSInvocationClassObject
    where cast (ObjCClassObject p) = NSInvocationClassObject (castPtr p)
instance Upcast NSInvocationClassObject ObjCClassObject
instance Downcast ObjCClassObject NSInvocationClassObject
_NSMethodSignature_ :: NSMethodSignatureClassObject
_NSMethodSignature_ = getClassObject
instance ObjCClass NSMethodSignatureClassObject
    where classObjectName _ = "NSMethodSignature"
instance Cast NSMethodSignatureClassObject ObjCClassObject
    where cast (NSMethodSignatureClassObject p) = ObjCClassObject (castPtr p)
instance Cast ObjCClassObject NSMethodSignatureClassObject
    where cast (ObjCClassObject p) = NSMethodSignatureClassObject (castPtr p)
instance Upcast NSMethodSignatureClassObject ObjCClassObject
instance Downcast ObjCClassObject NSMethodSignatureClassObject
_NSMovie_ :: NSMovieClassObject
_NSMovie_ = getClassObject
instance ObjCClass NSMovieClassObject
    where classObjectName _ = "NSMovie"
instance Cast NSMovieClassObject ObjCClassObject
    where cast (NSMovieClassObject p) = ObjCClassObject (castPtr p)
instance Cast ObjCClassObject NSMovieClassObject
    where cast (ObjCClassObject p) = NSMovieClassObject (castPtr p)
instance Upcast NSMovieClassObject ObjCClassObject
instance Downcast ObjCClassObject NSMovieClassObject
_NSMovieView_ :: NSMovieViewClassObject
_NSMovieView_ = getClassObject
instance ObjCClass NSMovieViewClassObject
    where classObjectName _ = "NSMovieView"
instance Cast NSMovieViewClassObject ObjCClassObject
    where cast (NSMovieViewClassObject p) = ObjCClassObject (castPtr p)
instance Cast ObjCClassObject NSMovieViewClassObject
    where cast (ObjCClassObject p) = NSMovieViewClassObject (castPtr p)
instance Upcast NSMovieViewClassObject ObjCClassObject
instance Downcast ObjCClassObject NSMovieViewClassObject
_NSNumber_ :: NSNumberClassObject
_NSNumber_ = getClassObject
instance ObjCClass NSNumberClassObject
    where classObjectName _ = "NSNumber"
instance Cast NSNumberClassObject ObjCClassObject
    where cast (NSNumberClassObject p) = ObjCClassObject (castPtr p)
instance Cast ObjCClassObject NSNumberClassObject
    where cast (ObjCClassObject p) = NSNumberClassObject (castPtr p)
instance Upcast NSNumberClassObject ObjCClassObject
instance Downcast ObjCClassObject NSNumberClassObject
_NSResponder_ :: NSResponderClassObject
_NSResponder_ = getClassObject
instance ObjCClass NSResponderClassObject
    where classObjectName _ = "NSResponder"
instance Cast NSResponderClassObject ObjCClassObject
    where cast (NSResponderClassObject p) = ObjCClassObject (castPtr p)
instance Cast ObjCClassObject NSResponderClassObject
    where cast (ObjCClassObject p) = NSResponderClassObject (castPtr p)
instance Upcast NSResponderClassObject ObjCClassObject
instance Downcast ObjCClassObject NSResponderClassObject
_NSString_ :: NSStringClassObject
_NSString_ = getClassObject
instance ObjCClass NSStringClassObject
    where classObjectName _ = "NSString"
instance Cast NSStringClassObject ObjCClassObject
    where cast (NSStringClassObject p) = ObjCClassObject (castPtr p)
instance Cast ObjCClassObject NSStringClassObject
    where cast (ObjCClassObject p) = NSStringClassObject (castPtr p)
instance Upcast NSStringClassObject ObjCClassObject
instance Downcast ObjCClassObject NSStringClassObject
_NSText_ :: NSTextClassObject
_NSText_ = getClassObject
instance ObjCClass NSTextClassObject
    where classObjectName _ = "NSText"
instance Cast NSTextClassObject ObjCClassObject
    where cast (NSTextClassObject p) = ObjCClassObject (castPtr p)
instance Cast ObjCClassObject NSTextClassObject
    where cast (ObjCClassObject p) = NSTextClassObject (castPtr p)
instance Upcast NSTextClassObject ObjCClassObject
instance Downcast ObjCClassObject NSTextClassObject
_NSTextField_ :: NSTextFieldClassObject
_NSTextField_ = getClassObject
instance ObjCClass NSTextFieldClassObject
    where classObjectName _ = "NSTextField"
instance Cast NSTextFieldClassObject ObjCClassObject
    where cast (NSTextFieldClassObject p) = ObjCClassObject (castPtr p)
instance Cast ObjCClassObject NSTextFieldClassObject
    where cast (ObjCClassObject p) = NSTextFieldClassObject (castPtr p)
instance Upcast NSTextFieldClassObject ObjCClassObject
instance Downcast ObjCClassObject NSTextFieldClassObject
_NSURL_ :: NSURLClassObject
_NSURL_ = getClassObject
instance ObjCClass NSURLClassObject
    where classObjectName _ = "NSURL"
instance Cast NSURLClassObject ObjCClassObject
    where cast (NSURLClassObject p) = ObjCClassObject (castPtr p)
instance Cast ObjCClassObject NSURLClassObject
    where cast (ObjCClassObject p) = NSURLClassObject (castPtr p)
instance Upcast NSURLClassObject ObjCClassObject
instance Downcast ObjCClassObject NSURLClassObject
_NSValue_ :: NSValueClassObject
_NSValue_ = getClassObject
instance ObjCClass NSValueClassObject
    where classObjectName _ = "NSValue"
instance Cast NSValueClassObject ObjCClassObject
    where cast (NSValueClassObject p) = ObjCClassObject (castPtr p)
instance Cast ObjCClassObject NSValueClassObject
    where cast (ObjCClassObject p) = NSValueClassObject (castPtr p)
instance Upcast NSValueClassObject ObjCClassObject
instance Downcast ObjCClassObject NSValueClassObject
_NSView_ :: NSViewClassObject
_NSView_ = getClassObject
instance ObjCClass NSViewClassObject
    where classObjectName _ = "NSView"
instance Cast NSViewClassObject ObjCClassObject
    where cast (NSViewClassObject p) = ObjCClassObject (castPtr p)
instance Cast ObjCClassObject NSViewClassObject
    where cast (ObjCClassObject p) = NSViewClassObject (castPtr p)
instance Upcast NSViewClassObject ObjCClassObject
instance Downcast ObjCClassObject NSViewClassObject
_HSProxy_ :: HSProxyClassObject
_HSProxy_ = getClassObject
instance ObjCClass HSProxyClassObject
    where classObjectName _ = "HSProxy"
instance Cast HSProxyClassObject ObjCClassObject
    where cast (HSProxyClassObject p) = ObjCClassObject (castPtr p)
instance Cast ObjCClassObject HSProxyClassObject
    where cast (ObjCClassObject p) = HSProxyClassObject (castPtr p)
instance Upcast HSProxyClassObject ObjCClassObject
instance Downcast ObjCClassObject HSProxyClassObject
instance ObjCTypeEncoding NSObjectClassObject
instance ObjCTypeEncoding NSArrayClassObject
instance ObjCTypeEncoding NSDictionaryClassObject
instance ObjCTypeEncoding NSInvocationClassObject
instance ObjCTypeEncoding NSMethodSignatureClassObject
instance ObjCTypeEncoding NSMovieClassObject
instance ObjCTypeEncoding NSMovieViewClassObject
instance ObjCTypeEncoding NSNumberClassObject
instance ObjCTypeEncoding NSResponderClassObject
instance ObjCTypeEncoding NSStringClassObject
instance ObjCTypeEncoding NSTextClassObject
instance ObjCTypeEncoding NSTextFieldClassObject
instance ObjCTypeEncoding NSURLClassObject
instance ObjCTypeEncoding NSValueClassObject
instance ObjCTypeEncoding NSViewClassObject
instance ObjCTypeEncoding HSProxyClassObject
instance ObjCArgument NSObjectClassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSArrayClassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSDictionaryClassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSInvocationClassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSMethodSignatureClassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSMovieClassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSMovieViewClassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSNumberClassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSResponderClassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSStringClassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSTextClassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSTextFieldClassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSURLClassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSValueClassObject
    where setArgument = setObjectArgument
instance ObjCArgument NSViewClassObject
    where setArgument = setObjectArgument
instance ObjCArgument HSProxyClassObject
    where setArgument = setObjectArgument
instance ObjCObjectArgument NSObjectClassObject
instance ObjCObjectArgument NSArrayClassObject
instance ObjCObjectArgument NSDictionaryClassObject
instance ObjCObjectArgument NSInvocationClassObject
instance ObjCObjectArgument NSMethodSignatureClassObject
instance ObjCObjectArgument NSMovieClassObject
instance ObjCObjectArgument NSMovieViewClassObject
instance ObjCObjectArgument NSNumberClassObject
instance ObjCObjectArgument NSResponderClassObject
instance ObjCObjectArgument NSStringClassObject
instance ObjCObjectArgument NSTextClassObject
instance ObjCObjectArgument NSTextFieldClassObject
instance ObjCObjectArgument NSURLClassObject
instance ObjCObjectArgument NSValueClassObject
instance ObjCObjectArgument NSViewClassObject
instance ObjCObjectArgument HSProxyClassObject
instance ObjCArguments NSObjectClassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSArrayClassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSDictionaryClassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSInvocationClassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSMethodSignatureClassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSMovieClassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSMovieViewClassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSNumberClassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSResponderClassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSStringClassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSTextClassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSTextFieldClassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSURLClassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSValueClassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSViewClassObject
    where setArguments = setObjectArgument 1
instance ObjCArguments HSProxyClassObject
    where setArguments = setObjectArgument 1
instance ObjCMessageReply NSObjectClassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSArrayClassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSDictionaryClassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSInvocationClassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSMethodSignatureClassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSMovieClassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSMovieViewClassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSNumberClassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSResponderClassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSStringClassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSTextClassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSTextFieldClassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSURLClassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSValueClassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSViewClassObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply HSProxyClassObject
    where sendMessage = sendObjectMessage
instance ObjCObjectMessageReply NSObjectClassObject
instance ObjCObjectMessageReply NSArrayClassObject
instance ObjCObjectMessageReply NSDictionaryClassObject
instance ObjCObjectMessageReply NSInvocationClassObject
instance ObjCObjectMessageReply NSMethodSignatureClassObject
instance ObjCObjectMessageReply NSMovieClassObject
instance ObjCObjectMessageReply NSMovieViewClassObject
instance ObjCObjectMessageReply NSNumberClassObject
instance ObjCObjectMessageReply NSResponderClassObject
instance ObjCObjectMessageReply NSStringClassObject
instance ObjCObjectMessageReply NSTextClassObject
instance ObjCObjectMessageReply NSTextFieldClassObject
instance ObjCObjectMessageReply NSURLClassObject
instance ObjCObjectMessageReply NSValueClassObject
instance ObjCObjectMessageReply NSViewClassObject
instance ObjCObjectMessageReply HSProxyClassObject
instance ObjCObject NSObject
instance Cast NSObject ID
    where cast (NSObject p) = ID (castPtr p)
instance Cast ID NSObject
    where cast (ID p) = NSObject (castPtr p)
instance Upcast NSObject ID
instance Downcast ID NSObject
instance ObjCObject NSArray
instance Cast NSArray ID
    where cast (NSArray p) = ID (castPtr p)
instance Cast ID NSArray
    where cast (ID p) = NSArray (castPtr p)
instance Upcast NSArray ID
instance Downcast ID NSArray
instance ObjCObject NSDictionary
instance Cast NSDictionary ID
    where cast (NSDictionary p) = ID (castPtr p)
instance Cast ID NSDictionary
    where cast (ID p) = NSDictionary (castPtr p)
instance Upcast NSDictionary ID
instance Downcast ID NSDictionary
instance ObjCObject NSInvocation
instance Cast NSInvocation ID
    where cast (NSInvocation p) = ID (castPtr p)
instance Cast ID NSInvocation
    where cast (ID p) = NSInvocation (castPtr p)
instance Upcast NSInvocation ID
instance Downcast ID NSInvocation
instance ObjCObject NSMethodSignature
instance Cast NSMethodSignature ID
    where cast (NSMethodSignature p) = ID (castPtr p)
instance Cast ID NSMethodSignature
    where cast (ID p) = NSMethodSignature (castPtr p)
instance Upcast NSMethodSignature ID
instance Downcast ID NSMethodSignature
instance ObjCObject NSMovie
instance Cast NSMovie ID
    where cast (NSMovie p) = ID (castPtr p)
instance Cast ID NSMovie
    where cast (ID p) = NSMovie (castPtr p)
instance Upcast NSMovie ID
instance Downcast ID NSMovie
instance ObjCObject NSMovieView
instance Cast NSMovieView ID
    where cast (NSMovieView p) = ID (castPtr p)
instance Cast ID NSMovieView
    where cast (ID p) = NSMovieView (castPtr p)
instance Upcast NSMovieView ID
instance Downcast ID NSMovieView
instance ObjCObject NSNumber
instance Cast NSNumber ID
    where cast (NSNumber p) = ID (castPtr p)
instance Cast ID NSNumber
    where cast (ID p) = NSNumber (castPtr p)
instance Upcast NSNumber ID
instance Downcast ID NSNumber
instance ObjCObject NSResponder
instance Cast NSResponder ID
    where cast (NSResponder p) = ID (castPtr p)
instance Cast ID NSResponder
    where cast (ID p) = NSResponder (castPtr p)
instance Upcast NSResponder ID
instance Downcast ID NSResponder
instance ObjCObject NSString
instance Cast NSString ID
    where cast (NSString p) = ID (castPtr p)
instance Cast ID NSString
    where cast (ID p) = NSString (castPtr p)
instance Upcast NSString ID
instance Downcast ID NSString
instance ObjCObject NSText
instance Cast NSText ID
    where cast (NSText p) = ID (castPtr p)
instance Cast ID NSText
    where cast (ID p) = NSText (castPtr p)
instance Upcast NSText ID
instance Downcast ID NSText
instance ObjCObject NSTextField
instance Cast NSTextField ID
    where cast (NSTextField p) = ID (castPtr p)
instance Cast ID NSTextField
    where cast (ID p) = NSTextField (castPtr p)
instance Upcast NSTextField ID
instance Downcast ID NSTextField
instance ObjCObject NSURL
instance Cast NSURL ID
    where cast (NSURL p) = ID (castPtr p)
instance Cast ID NSURL
    where cast (ID p) = NSURL (castPtr p)
instance Upcast NSURL ID
instance Downcast ID NSURL
instance ObjCObject NSValue
instance Cast NSValue ID
    where cast (NSValue p) = ID (castPtr p)
instance Cast ID NSValue
    where cast (ID p) = NSValue (castPtr p)
instance Upcast NSValue ID
instance Downcast ID NSValue
instance ObjCObject NSView
instance Cast NSView ID
    where cast (NSView p) = ID (castPtr p)
instance Cast ID NSView
    where cast (ID p) = NSView (castPtr p)
instance Upcast NSView ID
instance Downcast ID NSView
instance ObjCObject HSProxy
instance Cast HSProxy ID
    where cast (HSProxy p) = ID (castPtr p)
instance Cast ID HSProxy
    where cast (ID p) = HSProxy (castPtr p)
instance Upcast HSProxy ID
instance Downcast ID HSProxy
instance ObjCInstance NSObject
    where getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Cast NSObject ObjCInstanceObject
    where cast (NSObject p) = ObjCInstanceObject (castPtr p)
instance Cast ObjCInstanceObject NSObject
    where cast (ObjCInstanceObject p) = NSObject (castPtr p)
instance Upcast NSObject ObjCInstanceObject
instance Downcast ObjCInstanceObject NSObject
instance ObjCInstance NSArray
    where getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Cast NSArray ObjCInstanceObject
    where cast (NSArray p) = ObjCInstanceObject (castPtr p)
instance Cast ObjCInstanceObject NSArray
    where cast (ObjCInstanceObject p) = NSArray (castPtr p)
instance Upcast NSArray ObjCInstanceObject
instance Downcast ObjCInstanceObject NSArray
instance ObjCInstance NSDictionary
    where getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Cast NSDictionary ObjCInstanceObject
    where cast (NSDictionary p) = ObjCInstanceObject (castPtr p)
instance Cast ObjCInstanceObject NSDictionary
    where cast (ObjCInstanceObject p) = NSDictionary (castPtr p)
instance Upcast NSDictionary ObjCInstanceObject
instance Downcast ObjCInstanceObject NSDictionary
instance ObjCInstance NSInvocation
    where getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Cast NSInvocation ObjCInstanceObject
    where cast (NSInvocation p) = ObjCInstanceObject (castPtr p)
instance Cast ObjCInstanceObject NSInvocation
    where cast (ObjCInstanceObject p) = NSInvocation (castPtr p)
instance Upcast NSInvocation ObjCInstanceObject
instance Downcast ObjCInstanceObject NSInvocation
instance ObjCInstance NSMethodSignature
    where getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Cast NSMethodSignature ObjCInstanceObject
    where cast (NSMethodSignature p) = ObjCInstanceObject (castPtr p)
instance Cast ObjCInstanceObject NSMethodSignature
    where cast (ObjCInstanceObject p) = NSMethodSignature (castPtr p)
instance Upcast NSMethodSignature ObjCInstanceObject
instance Downcast ObjCInstanceObject NSMethodSignature
instance ObjCInstance NSMovie
    where getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Cast NSMovie ObjCInstanceObject
    where cast (NSMovie p) = ObjCInstanceObject (castPtr p)
instance Cast ObjCInstanceObject NSMovie
    where cast (ObjCInstanceObject p) = NSMovie (castPtr p)
instance Upcast NSMovie ObjCInstanceObject
instance Downcast ObjCInstanceObject NSMovie
instance ObjCInstance NSMovieView
    where getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Cast NSMovieView ObjCInstanceObject
    where cast (NSMovieView p) = ObjCInstanceObject (castPtr p)
instance Cast ObjCInstanceObject NSMovieView
    where cast (ObjCInstanceObject p) = NSMovieView (castPtr p)
instance Upcast NSMovieView ObjCInstanceObject
instance Downcast ObjCInstanceObject NSMovieView
instance ObjCInstance NSNumber
    where getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Cast NSNumber ObjCInstanceObject
    where cast (NSNumber p) = ObjCInstanceObject (castPtr p)
instance Cast ObjCInstanceObject NSNumber
    where cast (ObjCInstanceObject p) = NSNumber (castPtr p)
instance Upcast NSNumber ObjCInstanceObject
instance Downcast ObjCInstanceObject NSNumber
instance ObjCInstance NSResponder
    where getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Cast NSResponder ObjCInstanceObject
    where cast (NSResponder p) = ObjCInstanceObject (castPtr p)
instance Cast ObjCInstanceObject NSResponder
    where cast (ObjCInstanceObject p) = NSResponder (castPtr p)
instance Upcast NSResponder ObjCInstanceObject
instance Downcast ObjCInstanceObject NSResponder
instance ObjCInstance NSString
    where getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Cast NSString ObjCInstanceObject
    where cast (NSString p) = ObjCInstanceObject (castPtr p)
instance Cast ObjCInstanceObject NSString
    where cast (ObjCInstanceObject p) = NSString (castPtr p)
instance Upcast NSString ObjCInstanceObject
instance Downcast ObjCInstanceObject NSString
instance ObjCInstance NSText
    where getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Cast NSText ObjCInstanceObject
    where cast (NSText p) = ObjCInstanceObject (castPtr p)
instance Cast ObjCInstanceObject NSText
    where cast (ObjCInstanceObject p) = NSText (castPtr p)
instance Upcast NSText ObjCInstanceObject
instance Downcast ObjCInstanceObject NSText
instance ObjCInstance NSTextField
    where getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Cast NSTextField ObjCInstanceObject
    where cast (NSTextField p) = ObjCInstanceObject (castPtr p)
instance Cast ObjCInstanceObject NSTextField
    where cast (ObjCInstanceObject p) = NSTextField (castPtr p)
instance Upcast NSTextField ObjCInstanceObject
instance Downcast ObjCInstanceObject NSTextField
instance ObjCInstance NSURL
    where getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Cast NSURL ObjCInstanceObject
    where cast (NSURL p) = ObjCInstanceObject (castPtr p)
instance Cast ObjCInstanceObject NSURL
    where cast (ObjCInstanceObject p) = NSURL (castPtr p)
instance Upcast NSURL ObjCInstanceObject
instance Downcast ObjCInstanceObject NSURL
instance ObjCInstance NSValue
    where getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Cast NSValue ObjCInstanceObject
    where cast (NSValue p) = ObjCInstanceObject (castPtr p)
instance Cast ObjCInstanceObject NSValue
    where cast (ObjCInstanceObject p) = NSValue (castPtr p)
instance Upcast NSValue ObjCInstanceObject
instance Downcast ObjCInstanceObject NSValue
instance ObjCInstance NSView
    where getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Cast NSView ObjCInstanceObject
    where cast (NSView p) = ObjCInstanceObject (castPtr p)
instance Cast ObjCInstanceObject NSView
    where cast (ObjCInstanceObject p) = NSView (castPtr p)
instance Upcast NSView ObjCInstanceObject
instance Downcast ObjCInstanceObject NSView
instance ObjCInstance HSProxy
    where getClassObjectM _ = getClassObjectFromNameM "Object" >>=^ cast
instance Cast HSProxy ObjCInstanceObject
    where cast (HSProxy p) = ObjCInstanceObject (castPtr p)
instance Cast ObjCInstanceObject HSProxy
    where cast (ObjCInstanceObject p) = HSProxy (castPtr p)
instance Upcast HSProxy ObjCInstanceObject
instance Downcast ObjCInstanceObject HSProxy
instance ObjCTypeEncoding NSObject
instance ObjCTypeEncoding NSArray
instance ObjCTypeEncoding NSDictionary
instance ObjCTypeEncoding NSInvocation
instance ObjCTypeEncoding NSMethodSignature
instance ObjCTypeEncoding NSMovie
instance ObjCTypeEncoding NSMovieView
instance ObjCTypeEncoding NSNumber
instance ObjCTypeEncoding NSResponder
instance ObjCTypeEncoding NSString
instance ObjCTypeEncoding NSText
instance ObjCTypeEncoding NSTextField
instance ObjCTypeEncoding NSURL
instance ObjCTypeEncoding NSValue
instance ObjCTypeEncoding NSView
instance ObjCTypeEncoding HSProxy
instance ObjCArgument NSObject
    where setArgument = setObjectArgument
instance ObjCArgument NSArray
    where setArgument = setObjectArgument
instance ObjCArgument NSDictionary
    where setArgument = setObjectArgument
instance ObjCArgument NSInvocation
    where setArgument = setObjectArgument
instance ObjCArgument NSMethodSignature
    where setArgument = setObjectArgument
instance ObjCArgument NSMovie
    where setArgument = setObjectArgument
instance ObjCArgument NSMovieView
    where setArgument = setObjectArgument
instance ObjCArgument NSNumber
    where setArgument = setObjectArgument
instance ObjCArgument NSResponder
    where setArgument = setObjectArgument
instance ObjCArgument NSString
    where setArgument = setObjectArgument
instance ObjCArgument NSText
    where setArgument = setObjectArgument
instance ObjCArgument NSTextField
    where setArgument = setObjectArgument
instance ObjCArgument NSURL
    where setArgument = setObjectArgument
instance ObjCArgument NSValue
    where setArgument = setObjectArgument
instance ObjCArgument NSView
    where setArgument = setObjectArgument
instance ObjCArgument HSProxy
    where setArgument = setObjectArgument
instance ObjCObjectArgument NSObject
instance ObjCObjectArgument NSArray
instance ObjCObjectArgument NSDictionary
instance ObjCObjectArgument NSInvocation
instance ObjCObjectArgument NSMethodSignature
instance ObjCObjectArgument NSMovie
instance ObjCObjectArgument NSMovieView
instance ObjCObjectArgument NSNumber
instance ObjCObjectArgument NSResponder
instance ObjCObjectArgument NSString
instance ObjCObjectArgument NSText
instance ObjCObjectArgument NSTextField
instance ObjCObjectArgument NSURL
instance ObjCObjectArgument NSValue
instance ObjCObjectArgument NSView
instance ObjCObjectArgument HSProxy
instance ObjCArguments NSObject
    where setArguments = setObjectArgument 1
instance ObjCArguments NSArray
    where setArguments = setObjectArgument 1
instance ObjCArguments NSDictionary
    where setArguments = setObjectArgument 1
instance ObjCArguments NSInvocation
    where setArguments = setObjectArgument 1
instance ObjCArguments NSMethodSignature
    where setArguments = setObjectArgument 1
instance ObjCArguments NSMovie
    where setArguments = setObjectArgument 1
instance ObjCArguments NSMovieView
    where setArguments = setObjectArgument 1
instance ObjCArguments NSNumber
    where setArguments = setObjectArgument 1
instance ObjCArguments NSResponder
    where setArguments = setObjectArgument 1
instance ObjCArguments NSString
    where setArguments = setObjectArgument 1
instance ObjCArguments NSText
    where setArguments = setObjectArgument 1
instance ObjCArguments NSTextField
    where setArguments = setObjectArgument 1
instance ObjCArguments NSURL
    where setArguments = setObjectArgument 1
instance ObjCArguments NSValue
    where setArguments = setObjectArgument 1
instance ObjCArguments NSView
    where setArguments = setObjectArgument 1
instance ObjCArguments HSProxy
    where setArguments = setObjectArgument 1
instance ObjCMessageReply NSObject
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSArray
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSDictionary
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSInvocation
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSMethodSignature
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSMovie
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSMovieView
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSNumber
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSResponder
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSString
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSText
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSTextField
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSURL
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSValue
    where sendMessage = sendObjectMessage
instance ObjCMessageReply NSView
    where sendMessage = sendObjectMessage
instance ObjCMessageReply HSProxy
    where sendMessage = sendObjectMessage
instance ObjCObjectMessageReply NSObject
instance ObjCObjectMessageReply NSArray
instance ObjCObjectMessageReply NSDictionary
instance ObjCObjectMessageReply NSInvocation
instance ObjCObjectMessageReply NSMethodSignature
instance ObjCObjectMessageReply NSMovie
instance ObjCObjectMessageReply NSMovieView
instance ObjCObjectMessageReply NSNumber
instance ObjCObjectMessageReply NSResponder
instance ObjCObjectMessageReply NSString
instance ObjCObjectMessageReply NSText
instance ObjCObjectMessageReply NSTextField
instance ObjCObjectMessageReply NSURL
instance ObjCObjectMessageReply NSValue
instance ObjCObjectMessageReply NSView
instance ObjCObjectMessageReply HSProxy
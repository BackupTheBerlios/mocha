{-# OPTIONS -fglasgow-exts #-}

module Mocha.AutoMarshal where

import Data.Dynamic
import Data.IORef
import Foreign.C.String

import Mocha.Base
import Mocha.Messaging
import Mocha.TypeEncodings

-- FIXME: this should really be [a], not [Char] ...
instance ObjCArgument String where
   setArgument i expr argument =
      -- automarshal a String as an NSString
      Foreign.C.String.withCString argument (cMarshalCStringAsNSString i expr)
instance ObjCArguments String where
   setArguments = setArgument 1

{-
instance (ObjCAutoMarshal a b, ObjCTypeEncoding b) => ObjCArgument a where
   setArgument i expr argument = setArgument i expr (marshal argument :: b)

instance ObjCArgument haskellType => ObjCAutoMarshal haskellType objCType where
   automarshal :: haskellType -> objCType

instance ObjCAutoMarshal String NSString where
   automarshal = 
-}

{-
instance (Typeable a, Show a) => ObjCArgument [a] where
   setArgument i expr argument = return ()
   -- FIXME: marshal arrays
-}

instance ObjCArgument ref => ObjCArgument (IORef ref) where
   setArgument i expr argument = do
      ref <- readIORef argument
      setArgument i expr ref

instance ObjCMessageReply Selector where
   sendMessage methodName args receiver = do
      selectorCString <- sendAMessage cSendSelectorMessage methodName
	 args receiver
      s <- peekCString selectorCString
      return (Selector s)

{-
class (ObjCArgument from, ObjCArgument to) => ObjCAutoMarshal from to where
   marshal :: from -> to

instance NSString String where
   marshal = 
   sendMessage methodName args receiver = do
      nsString <- sendAMessage cSendIDMessage methodName args receiver
      cString <- cMarshalNSStringToCString nsString
      peekCString cString
-}


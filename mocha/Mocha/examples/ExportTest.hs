module ExportTest where

import Data.IORef
import Foreign.C
import Foreign.Ptr

import Base
import ExportHelper
import InterfaceGenerator
import Messaging

$(wholeHierarchyQ)

myObject = MochaClass
	 { messageHandler = receivedMessage
	 , rts = respondsToSelector
	 , msfs = methodSignatureForSelector
	 , className = "MyObject"
	 , superclassName = "NSObject"
	 , messagesUnderstood =
	   [("rot13:", reifyType rot13)]
	 , instanceVariables = [aTextField]
	 }


aTextField = newInstanceVariable "aTextField" nil

methodSignatureForSelector :: CString -> IO CString
methodSignatureForSelector selector = newCString "v@:"

respondsToSelector :: CString -> IO Bool
respondsToSelector cSelector = do
   selector <- peekCString cSelector
   case selector of
      ("rot13:") -> return True
      ("setATextField:") -> return True

rot13 :: ID -> IO ()
rot13 _ = do
   putStrLn $ "hi!"

receivedMessage :: NSInvocationInstance -> IO ()
receivedMessage invocation = return ()

__mocha_class_MyObject surrogate =
   mkClass (initialiseMochaClass myObject surrogate)

__mocha_className_MyObject = newCString "MyObject"

__mocha_superclassName_MyObject = newCString "NSObject"

foreign export ccall
   __mocha_class_MyObject :: HSProxyInstance
			  -> IO (FunPtr InitialiseMochaClass)

foreign export ccall
   __mocha_className_MyObject :: IO CString

foreign export ccall
   __mocha_superclassName_MyObject :: IO CString

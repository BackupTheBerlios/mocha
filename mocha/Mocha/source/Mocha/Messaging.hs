{-# OPTIONS -fglasgow-exts #-}

module Mocha.Messaging

where

import Data.Dynamic hiding (cast)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import Mocha.Base
import Mocha.MessagingTemplates
import Mocha.TypeEncodings


{-
   instance (ObjCTypeEncoding a, ObjCTypeEncoding b)
	  => ObjCTypeEncoding (a, b) where
      typeEncoding (a, b) = typeEncoding a ++ typeEncoding b
   instance (ObjCTypeEncoding a, ObjCTypeEncoding b, ObjCTypeEncoding c)
	  => ObjCTypeEncoding (a, b, c) where
      typeEncoding (a, b, c) = typeEncoding a ++ typeEncoding b ++ typeEncoding c
-}

$(declareTypeEncodingForTuples [2..10])

{-
   instance (ObjCArgument a, ObjCArgument b) => ObjCArguments (a, b) where
      setArguments expr (a, b) = do
	 setArgument 1 expr a
	 setArgument 2 expr b
    
   instance (ObjCArgument a, ObjCArgument b, ObjCArgument c)
	 => ObjCArguments (a, b, c) where
      setArguments expr (a, b, c) = do
	 setArgument 1 expr a
	 setArgument 2 expr b
	 setArgument 3 expr c
-}

$(declareObjCArgumentsForTuples [2..10])




newtype ObjCMessageExpression =
   ObjCMessageExpression (Ptr ObjCMessageExpression)

class ObjCTypeEncoding arg => ObjCArgument arg where
   setArgument :: Int -> ObjCMessageExpression -> arg -> IO ()

class ObjCTypeEncoding args => ObjCArguments args where
   setArguments :: ObjCMessageExpression -> args -> IO ()

class ObjCTypeEncoding reply => ObjCMessageReply reply where
   sendMessage :: ObjCArguments args => String -> args -> ID -> IO reply

-- for objects
class ObjCObject arg => ObjCObjectArgument arg where
   setObjectArgument :: Cast arg ID => Int -> ObjCMessageExpression -> arg -> IO ()
   setObjectArgument index expr arg = cSetIDArgument index expr (cast arg :: ID)

class ObjCObject reply => ObjCObjectMessageReply reply where
   sendObjectMessage :: (ObjCArguments args) => String -> args -> ID -> IO reply
   sendObjectMessage methodName args receiver = do
      id <- sendAMessage cSendIDMessage methodName args receiver :: IO ID 
      return $ cast id

{-
class ObjCTypeEncoding a => GoesToObjC a

class ObjCTypeEncoding a => ComesFromObjC a

class (GoesToObjC haskellType, ComesFromObjC objCType)
   => ObjCMarshalFromHaskellToObjC haskellType objCType
   marshalToObjC :: haskellType -> objCType

class (GoesToObjC haskellType, ComesFromObjC objCType)
   => ObjCMarshalFromObjCToHaskell objCType haskellType
   marshalFromObjC :: objCType -> haskellType
-}


instance ObjCArgument Char where
   setArgument _ _ _ = error "erk"
instance ObjCArguments Char where
   setArguments = setArgument 1
instance ObjCMessageReply Char where
   sendMessage _ _ _ = error "char erk"

instance ObjCArgument Bool where
   setArgument = cSetBoolArgument
instance ObjCArguments Bool where
   setArguments = setArgument 1
instance ObjCMessageReply Bool where
   sendMessage = sendAMessage cSendBoolMessage

instance ObjCArgument (Ptr x) where
   setArgument i e a = cSetVoidPtrArgument i e (castPtr a :: Ptr ())
instance ObjCArguments (Ptr x) where
   setArguments = setArgument 1

instance ObjCArgument (FunPtr x) where
   setArgument i e a = setArgument i e (castFunPtrToPtr a :: Ptr ())
instance ObjCArguments (FunPtr x) where
   setArguments = setArgument 1

instance ObjCArgument Int where
   setArgument = cSetIntArgument
instance ObjCArguments Int where
   setArguments = setArgument 1
instance ObjCMessageReply Int where
   sendMessage = sendAMessage cSendIntMessage

instance ObjCArgument CUInt where
   setArgument i e a = cSetIntArgument i e (fromIntegral a :: Int)
instance ObjCArguments CUInt where
   setArguments = setArgument 1
--instance ObjCMessageReply CUInt where
--   sendMessage = sendAMessage cSendIntMessage

instance ObjCArgument ID where
   setArgument = cSetIDArgument
instance ObjCArguments ID where
   setArguments = setArgument 1
instance ObjCMessageReply ID where
   sendMessage = sendAMessage cSendIDMessage

instance ObjCMessageReply String where
   sendMessage methodName args receiver = do
      nsString <- sendAMessage cSendIDMessage methodName args receiver
      cString <- cMarshalNSStringToCString nsString
      peekCString cString

instance ObjCMessageReply CString where
   sendMessage = sendAMessage cSendCStringMessage

instance ObjCArguments () where
   setArguments _ () = return ()
instance ObjCMessageReply () where
   sendMessage = sendAMessage cSendVoidMessage

sendAMessage :: (ObjCArguments args, ObjCMessageReply reply)
	     => (ObjCMessageExpression -> IO reply)
	     -> String -> args -> ID -> IO reply
sendAMessage messagingFunction methodName arguments receiver =
   buildMessageExpression methodName arguments receiver >>=
   messagingFunction


-- Convenience functions to pass in IO types as arguments
instance (Typeable a, ObjCArgument a) => ObjCArgument (IO a) where
   setArgument i e argAction = do
      a <- argAction
      setArgument 1 e a

instance (Typeable a, ObjCArgument a) => ObjCArguments (IO a) where
   setArguments e argAction = do
      a <- argAction
      setArgument 1 e a


-- FIXME: Templatise this


--instance ObjCTypeEncoding Selector where typeEncoding _ = ":"
-- arrays
--instance (ObjCTypeEncoding a) => ObjCTypeEncoding [a]
--   where typeEncoding list = concat $ map typeEncoding list
-- structs
-- unions
-- bitfields
-- pointers

argumentsTypeEncoding receiver methodName = do
   typeEncodingCString <- newCString methodName
   typeEncoding <- cArgumentsObjCTypeEncoding receiver typeEncodingCString
   peekCString typeEncoding

returnTypeEncoding receiver methodName = do
   typeEncodingCString <- newCString methodName
   typeEncoding <- cReturnObjCTypeEncoding receiver typeEncodingCString
   peekCString typeEncoding


buildMessageExpression methodName arguments receiver = do
   typeOfMethodArguments <- argumentsTypeEncoding receiver methodName
   let typeOfSendingArguments = typeEncoding arguments
   if (typeOfMethodArguments == typeOfSendingArguments)
      then do
	 messageExpression <- makeMessageExpression receiver methodName
	 setArguments messageExpression arguments
	 return messageExpression
      else do
	 -- XXX: Should also output method name/receiving object
	 ioError (userError $
	    "Message arguments type encoding for method name \"" ++
	    methodName ++ "\" (receiving object is \"" ++ show receiver ++
	    "\") is \"" ++
	    typeOfSendingArguments ++ "\", expecting \"" ++
	    typeOfMethodArguments ++ "\"")





-- (#) :: obj -> (obj -> reply) -> reply
object # method = method object
infixl 9 #

-- FIXME: typecheck return messages

makeMessageExpression receiver methodName = do
   withCString methodName (cMakeMessageExpression receiver)

foreign import ccall safe "MochaBridge.h argumentsTypeEncoding"
   cArgumentsObjCTypeEncoding :: ID -> CString -> IO CString
foreign import ccall safe "MochaBridge.h returnTypeEncoding"
   cReturnObjCTypeEncoding :: ID -> CString -> IO CString
foreign import ccall safe "MochaBridge.h makeMessageExpression"
   cMakeMessageExpression :: ID -> CString -> IO ObjCMessageExpression

foreign import ccall safe "MochaBridge.h setIDArgument"
   cSetIDArgument :: Int -> ObjCMessageExpression -> ID -> IO ()
foreign import ccall safe "MochaBridge.h setIntArgument"
   cSetIntArgument :: Int -> ObjCMessageExpression -> Int -> IO ()
foreign import ccall safe "MochaBridge.h setBoolArgument"
   cSetBoolArgument :: Int -> ObjCMessageExpression -> Bool -> IO ()
foreign import ccall safe "MochaBridge.h setVoidPtrArgument"
   cSetVoidPtrArgument :: Int -> ObjCMessageExpression -> Ptr () -> IO ()
foreign import ccall safe "MochaBridge.h marshalCStringAsNSString"
   cMarshalCStringAsNSString :: Int -> ObjCMessageExpression -> CString -> IO ()

foreign import ccall safe "MochaBridge.h sendVoidMessage"
   cSendVoidMessage :: ObjCMessageExpression -> IO ()
foreign import ccall safe "MochaBridge.h sendIntMessage"
   cSendIntMessage :: ObjCMessageExpression -> IO Int
foreign import ccall safe "MochaBridge.h sendBoolMessage"
   cSendBoolMessage :: ObjCMessageExpression -> IO Bool
foreign import ccall safe "MochaBridge.h sendIDMessage"
   cSendIDMessage :: ObjCMessageExpression -> IO ID
foreign import ccall safe "MochaBridge.h sendSelectorMessage"
   cSendSelectorMessage :: ObjCMessageExpression -> IO CString
   -- FIXME: above should be "IO Selector"
foreign import ccall safe "MochaBridge.h sendCStringMessage"
   cSendCStringMessage :: ObjCMessageExpression -> IO CString

foreign import ccall safe "MochaBridge.h marshalNSStringToCString"
   cMarshalNSStringToCString :: ID -> IO CString



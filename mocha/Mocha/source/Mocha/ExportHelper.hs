{-# OPTIONS -fglasgow-exts #-}

module Mocha.ExportHelper 
   ( mochaExportClass
   , newInstanceVariable
   , setInstanceVariable
   , getInstanceVariable
   , getInstanceObject
   , InstanceVariable
   , modifyInstanceVariable
   , ForwardInvocation
   , RespondsToSelector
   , MethodSignatureForSelector
   , mkFICallback
   , mkRTSCallback
   , mkMSFSCallback
   )
where

import Data.Dynamic hiding (cast)
import Data.FiniteMap
import Data.IORef
import Foreign
import Foreign.C
import Language.Haskell.THSyntax
import Prelude hiding (init)

import Mocha.Base
import Mocha.DirectMessaging
import Mocha.ExportTemplates
-- import InterfaceGenerator
import Mocha.Messaging
import Mocha.ReifyType
import Mocha.TypeEncodings

-- $(wholeHierarchyQ)

{-
mochaClass :: MochaClassInterface -> MochaClass
mochaClass i@(Interface { methods = m
                      , className = n
		      , superclassName = scn
		      }) =
   MochaClass
   { messageHandler = undefined -- $(makeMessageHandler m)
   , rts = $(makeRespondsToSelector m)
   , msfs = $(makeMethodSignatureForSelector m)
   , methodMap = makeMethodMap m
   , className' = n
   , superclassName' = scn
   }
-}

mochaExportClass :: MochaClassInterface -> Q [Dec]
mochaExportClass (Interface { methods = m
                            , instanceVariables = iv
			    , className = n
			    , superclassName = scn
			    }) = Q $ return
   (
      [
	 -- respondsToSelector = makeRespondsToSelector m
	 ValD (VarP "respondsToSelector") (NormalB (makeRespondsToSelector m iv)) []
	 ,
	 -- methodSignatureForSelector = makeMethodSignatureForSelector m
	 ValD (VarP "methodSignatureForSelector")
	    (NormalB (makeMethodSignatureForSelector m iv)) []
	 ,
	 -- __mocha_className_MyObject = newCString "$n"
	 ValD (VarP $ "__mocha_className_" ++ n)
	    (NormalB (AppE (VarE "Foreign.C.String:newCString")
			 (LitE (StringL n)))) []
	 ,
	 -- __mocha_superclassName_MyObject = newCString "$scn"
	 ValD (VarP $ "__mocha_superclassName_" ++ n)
	    (NormalB (AppE (VarE "Foreign.C.String:newCString")
			 (LitE (StringL scn)))) []
	 ,
	 -- __mocha_class_$n surrogate = do
	 --    fiCallback <- mkFICallback mh
	 --    rtsCallback <- mkRTSCallback rts'
	 --    msfsCallback <- mkMSFSCallback msfs'
	 --    __mocha_initialiseSurrogate surrogate
	 --       fiCallback rtsCallback msfsCallback
	 FunD ("__mocha_class_" ++ n)
	 [
	    Clause [VarP "surrogate"] (NormalB
	       (
		  DoE
		  [
		     -- fiCallback <- mkFICallback mh
		     BindS (VarP "fiCallback") $
			AppE (VarE "mkFICallback")
			    (VarE "receivedMessage") -- XXX: replace with real one
		     ,
		     -- rtsCallback <- mkRTSCallback respondsToSelector
		     BindS (VarP "rtsCallback") $
			AppE (VarE "mkRTSCallback")
			    (VarE "respondsToSelector")
		     ,
		     -- rtsCallback <- mkRTSCallback respondsToSelector
		     BindS (VarP "msfsCallback") $
			AppE (VarE "mkMSFSCallback")
			    (VarE "methodSignatureForSelector")
		     ,
		     -- __mocha_initialiseSurrogate surrogate
		     --	fiCallback rtsCallback msfsCallback
		     NoBindS (AppE (AppE (AppE (AppE (VarE
			"__mocha_initialiseSurrogate") (VarE "surrogate"))
			(VarE "fiCallback")) (VarE "rtsCallback"))
			(VarE "msfsCallback"))
		     ,
		     -- putStrLn "initialiseMochaClass ($n) called"
		     NoBindS (AppE (VarE "putStrLn")
			(LitE (StringL $
			      "initialiseMochaClass (" ++ n ++ ") called")))
		  ]
	       )
	    )
	    []
	 ]
	 ,
	 let
	    exportedName = "__mocha_class_" ++ n
	    hsProxyToIOT = tappsArr [ConT "HSProxy",
				     AppT (ConT "GHC.IOBase:IO") (TupleT 0)]
	 in
	 -- foreign export ccall __mocha_class_MyObject :: HSProxy -> IO ()
	 ForeignD (ExportF CCall exportedName exportedName hsProxyToIOT)
	 ,
	 -- foreign export ccall __mocha_className_MyObject :: IO CString
	 let
	    exportedName = "__mocha_className_" ++ n
	    ioCStringT = AppT (ConT "GHC.IOBase:IO") (ConT "Foreign.C.String:CString")
	 in
	 ForeignD (ExportF CCall exportedName exportedName ioCStringT)
	 ,
	 -- foreign export ccall __mocha_superclassName_MyObject :: IO CString
	 let
	    exportedName = "__mocha_superclassName_" ++ n
	    ioCStringT = AppT (ConT "GHC.IOBase:IO") (ConT "Foreign.C.String:CString")
	 in
	 ForeignD (ExportF CCall exportedName exportedName ioCStringT)
	 ,
	 -- foreign import ccall "HSProxy.h __mocha_initialiseSurrogate"
	 --    __mocha_initialiseSurrogate :: HSProxy
	 -- 				-> FunPtr (NSInvocation -> IO ())
	 --				-> FunPtr (CString -> IO Bool)
	 --				-> FunPtr (CString -> IO CString)
	 --				-> IO ()
	 ForeignD (ImportF CCall Safe "__mocha_initialiseSurrogate"
	    "__mocha_initialiseSurrogate"
	    (
	       tappsArr
	       [
		  ConT "HSProxy",
		  mkFunPtrT $ tappsArr [ConT "NSInvocation", (mkIOT (TupleT 0))],
		  mkFunPtrT $ tappsArr [ConT "Foreign.C.String:CString",
					(mkIOT (ConT "GHC.Base:Bool"))],
		  mkFunPtrT $ tappsArr [ConT "Foreign.C.String:CString",
					(mkIOT (ConT "Foreign.C.String:CString"))],
		  mkIOT (TupleT 0)
	       ]
	    )
	 )
	 ,
	 ForeignD (ImportF CCall Safe "getIDArgument" "__mocha_getIDArgument"
	    (
	       tappsArr [ConT "GHC.Base:Int",
			 ConT "IncomingMessage",
			 mkIOT (ConT "ID")]
	    )
	 )
      ]
      ++
      makeReceivedMessage m iv
   )

mkFunPtrT t = AppT (ConT "GHC.Ptr:FunPtr") t

mkIOT t = AppT (ConT "GHC.IOBase:IO") t

tappsArr :: [Type] -> Type
tappsArr = foldr1 (\x y -> AppT (AppT  ArrowT x) y)

{-
makeMethodMap [] = emptyFM :: FiniteMap String MethodInvocation
makeMethodMap (Method selector method:x) =
   addToFM (makeMethodMap x) selector (MethodInvocation method)
-}

initialiseMochaClass :: MochaClass -> HSProxy -> IO ()
initialiseMochaClass (MochaClass { messageHandler = mh
			         , rts = rts'
				 , msfs = msfs'
				 , className' = cn
				 , superclassName' = scn
				 }) surrogate = do
   putStrLn "initialiseMochaClass called"
   fiCallback <- mkFICallback mh
   rtsCallback <- mkRTSCallback rts'
   msfsCallback <- mkMSFSCallback msfs'
   -- __mocha_initialiseSurrogate surrogate fiCallback rtsCallback msfsCallback
   return ()


-- Global variables using the unsafePerformIO (newIORef x) hack
-- see http://www.mail-archive.com/haskell-cafe@haskell.org/msg02506.html

-- Instance Variables
type InstanceVariable = Dynamic

{-# NOINLINE newInstanceVariable #-}
newInstanceVariable :: Typeable a => String -> a -> InstanceVariable
newInstanceVariable name value = toDyn (unsafePerformIO (newIORef value))

setInstanceVariable i = writeIORef (fromDyn i (error "setInstanceVariable"))

getInstanceVariable i = readIORef (fromDyn i (error "getInstanceVariable"))

modifyInstanceVariable i = modifyIORef
   (fromDyn i (error "modifyInstanceVariable"))

getInstanceObject :: Cast ID to => InstanceVariable -> IO to
getInstanceObject iv = do
   object <- getInstanceVariable iv :: IO ID
   return (cast object)

{-
type InstanceVariable x = IORef x

mkGlobalVar :: String -> a -> IORef a
mkGlobalVar name value = unsafePerformIO (newIORef value)

newInstanceVariable = mkGlobalVar

setInstanceVariable = writeIORef

getInstanceVariable = readIORef

modifyInstanceVariable = modifyIORef
-}
-- respondsToSelector

-- Forward Invocation callback
type ForwardInvocation = NSInvocation -> IO ()
foreign import ccall "wrapper"
   mkFICallback :: ForwardInvocation -> IO (FunPtr ForwardInvocation)

type RespondsToSelector = CString -> IO Bool 
foreign import ccall "wrapper"
   mkRTSCallback :: RespondsToSelector -> IO (FunPtr RespondsToSelector)

type MethodSignatureForSelector = CString -> IO CString
foreign import ccall "wrapper"
   mkMSFSCallback :: MethodSignatureForSelector
	          -> IO (FunPtr MethodSignatureForSelector)

type InitialiseMochaClass = IO ()
foreign import ccall "wrapper"
  mkClass :: InitialiseMochaClass -> IO (FunPtr InitialiseMochaClass)


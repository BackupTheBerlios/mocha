{-# OPTIONS -fglasgow-exts #-}

module Mocha.ExportTemplates
{-
   (
   , MethodDeclaration(..)
   , MethodInvocation(..)
   , MochaClassInterface(..)
   , MochaClass(..)
   , makeReceivedMessage
   , makeRespondsToSelectorQ
   , makeRespondsToSelector
   , makeMethodSignatureForSelectorQ
   , makeMethodSignatureForSelector
   , rot13
   )
-}

where

import Char
import Data.Dynamic
import Data.FiniteMap
import Foreign.C.String
import Foreign.Ptr
import Language.Haskell.THSyntax

import Mocha.Base
import Mocha.ExportTemplatesHelper
import Mocha.InterfaceGenerator
import Mocha.Messaging
import Mocha.ReifyType
import Mocha.TypeEncodings

$(wholeHierarchyQ) -- only need NSInvocation, actually ...

data MethodDeclaration = Method String TypeQ

data MethodInvocation = MethodInvocation ExpQ

data MochaClassInterface = Interface
			   { methods :: [MethodDeclaration]
                           , instanceVariables :: [String]
			   , className :: String
			   , superclassName :: String
			   }

data MochaClass = MochaClass
	          { messageHandler :: NSInvocation -> IO ()
		  , rts :: CString -> IO Bool
		  , msfs :: CString -> IO CString
		  -- , methodMap :: FiniteMap String MethodInvocation
		  , className' :: String
		  , superclassName' :: String
		  }


makeRespondsToSelectorQ =
   \methodDeclarations instanceVariables ->
      Q (return $ makeRespondsToSelector methodDeclarations)

makeRespondsToSelector methodDeclarations instanceVariables =
   (
      -- \cSelector -> ...
      LamE [VarP "cSelector"]
      (
	 -- do ...
	 DoE
	 [
	    -- selector <- peekCString cSelector
	    BindS (VarP "selector") (AppE (VarE "Foreign.C.String:peekCString")
					  (VarE "cSelector"))
	    ,
	    -- case selector of ...
	    NoBindS
	    (
	       CaseE (VarE "selector") $
	       -- foreach ("$selector" -> return True)
	       [ returnTrueForSelector selector |
		 Method selector _ <- methodDeclarations ] ++
               -- foreach ("set${instanceVariable}" -> return True)
               [ setTheInstanceVariable instanceVariable |
                 instanceVariable <- instanceVariables ] ++
               -- _ -> return False
	       [ Match WildP (NormalB (AppE (VarE "return")
	                                  (ConE "GHC.Base:False"))) [] ]
	    )
	 ]
      )
   )
   where
      -- $selector -> return True
      returnTrueForSelector selector =
	 Match (LitP (StringL selector))
	       (NormalB (AppE (VarE "return")
		       (ConE "GHC.Base:True"))) []
      -- "set${instanceVariable}:" -> return True
      setTheInstanceVariable instanceVariable =
         Match (LitP (StringL $ "set" ++ capitalise instanceVariable ++ ":"))
	       (NormalB (AppE (VarE "return")
		       (ConE "GHC.Base:True"))) []

capitalise :: String -> String
capitalise "" = ""
capitalise (a:x) = Char.toUpper a:x

makeMethodSignatureForSelectorQ methodDeclarations instanceVariables =
   Q (return $ makeMethodSignatureForSelector methodDeclarations)

-- Thank God for GHC automatically deriving instances!
-- instance Eq Type
-- instance Eq (Q Type)

makeMethodSignatureForSelector methodDeclarations instanceVariables =
   (
      -- \cSelector -> ...
      LamE [VarP "cSelector"]
      (
	 -- do ...
	 DoE
	 [
	    -- selector <- peekCString cSelector
	    BindS (VarP "selector") (AppE (VarE "Foreign.C.String:peekCString")
					  (VarE "cSelector"))
	    ,
	    -- case selector of ...
	    NoBindS
	    (
	       CaseE (VarE "selector") $
	       -- foreach ($selector -> newCString "$typeEncoding")
	       [ typeEncodingFor selector typeQ |
		 Method selector typeQ <- methodDeclarations ] ++
               -- 
               [ typeEncodingForTheInstanceVariable instanceVariable |
                 instanceVariable <- instanceVariables ]
	    )
	 ]
      )
   )
   where
      -- $selector -> newCString "$typeEncoding"
      typeEncodingFor selector typeQ =
	 {-
	 -- need to typecheck whether the type signatures match or not
	 if typeQ == (returnQ $ rType (objCSelectorExprToHaskellFunctionName [| selector |])) then
	 -}
		     Match (LitP (StringL selector))
			   (NormalB (AppE (VarE "Foreign.C.String:newCString")
				   (LitE (StringL (typeEncoding typeQ))))) []
	 {-
		  else
		     error "aieee"
	 -}
      typeEncodingForTheInstanceVariable instanceVariable =
	 Match (LitP (StringL $ "set" ++ (capitalise instanceVariable) ++ ":"))
	       (NormalB (AppE (VarE "Foreign.C.String:newCString")
	               (LitE (StringL ("v@:@"))))) []

makeReceivedMessage methodDeclarations instanceVariables =
   [
      -- type IncomingMessage = NSInvocation
      TySynD "IncomingMessage" [] (ConT "NSInvocation")
      ,
      FunD "receivedMessage" [Clause [VarP "invocation"]
      (NormalB $ 
	 -- do
	 DoE
	 [
	    -- sel <- selector invocation ()
	    BindS (VarP "sel") (AppE (AppE (VarE "selector") (ConE "GHC.Base:()"))
				     (VarE "invocation"))
	    ,
	    -- case sel_getName sel of
	    NoBindS $ CaseE (AppE (VarE "sel_getName") (VarE "sel"))
	    (
	       -- "set$foo" -> setInstanceVariable foo
	       (map makeSetInstanceVariableMatch instanceVariables)
	       ++
	       -- "$methodName" -> runAction foo
	       (map makeMethod methodDeclarations)
	    )
	 ]
      ) []]
   ]

makeSetInstanceVariableMatch instanceVariable =
   Match (LitP $ StringL ("set" ++ (capitalise instanceVariable) ++ ":"))
   (NormalB
      (
	 -- "set$foo:" -> do
	 DoE
	 [
	    -- arg <- __mocha_getIDArgument 1 invocation
	    getInvocationArgument,
	    -- setInstanceVariable $foo arg
	    NoBindS $ AppE (AppE (VarE "setInstanceVariable") (VarE instanceVariable))
		 (VarE "arg")
	 ]
      )
   ) []

getInvocationArgument =
   BindS (VarP "arg") (AppE (AppE (VarE "__mocha_getIDArgument")
      (LitE (IntegerL 1))) (VarE "invocation"))

makeMethod (Method selectorName methodType) =
   Match (LitP $ StringL selectorName)
   (NormalB
      (
	 -- "$selectorName:" -> do
	 DoE
	 [
	    -- arg <- __mocha_getIDArgument 1 invocation
	    getInvocationArgument,
	    -- $action arg
	    NoBindS $ AppE (VarE (objCSelectorNameToHaskellFunctionName
				  selectorName))
		           (VarE "arg")
	 ]
      )
   ) []




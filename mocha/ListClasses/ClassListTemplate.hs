module ClassListTemplate where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Language.Haskell.THSyntax

import System.Environment

type Class = Ptr ()

templateLookup = do
   qIO (lookupClasses)
   bleh
   
bleh = [d| data Foo = Foo |]

lookupClasses = do
   initialiseClassLookup
   c <- getNextClass
   lookupClass c
   p <- getProgName
   putStrLn $ "Program name is \"" ++ p ++ "\"."

lookupClass c
   | c == nullPtr = return ()
   | otherwise	  = do
     cnCS <- className c
     cn <- peekCString cnCS
     scnCS <- superclassName c
     scn <- peekCString scnCS
     putStrLn $ cn ++ "(" ++ scn ++ ")"
     newC <- getNextClass
     lookupClass newC

foreign import ccall "initialise_class_lookup"
   initialiseClassLookup :: IO ()

foreign import ccall "get_next_class"
   getNextClass :: IO Class

foreign import ccall "class_name"
   className :: Class -> IO CString

foreign import ccall "superclass_name"
   superclassName :: Class -> IO CString


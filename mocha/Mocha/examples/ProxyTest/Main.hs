{-# OPTIONS -fglasgow-exts #-}

module Main where

import Data.List (genericLength)
import System.Environment
import Foreign
import Foreign.C

-- XXX: Seems to crash if passed two to three arguments on the commandline.
-- Weeeeeird.

nsApplicationMain prog args = do
   withMany withCString (prog : args) $ \argvPtrs ->
    withArray0 nullPtr argvPtrs $ \argvBuf ->
     c_nsApplicationMain (1 + genericLength args) argvBuf
{-
   foo <- newCString "foo"
   argvBuf <- newArray0 nullPtr [foo]
   c_nsApplicationMain 1 argvBuf
-}

main = do
    nsApplicationMain "me" ["foo", "bar"]
    
foreign import ccall safe "NSApplicationMain.h NSApplicationMain"
   c_nsApplicationMain :: CInt -> Ptr CString -> IO CInt


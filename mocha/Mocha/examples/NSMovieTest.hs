{-# OPTIONS -fglasgow-exts #-}

module Main where

import Prelude hiding (init)

import Foreign.C
import Foreign.Ptr

import Mocha
import DirectMessagingExample

main :: IO ()
main = withAutoreleasePool loadDemoMovie

loadDemoMovie = do
   url <- _NSURL_ # alloc () >>= initWithString "file:///Users/andrep/Movies/powerbook_g4-ad_m480.mov"
   --
   urlText <- url # absoluteString ()
   putStrLn (urlText)
   putStrLn (map succ urlText)
   --
   mov <- _NSMovie_ # alloc () >>= initWithURL_byReference (url, False)
   putStrLn $ "NSMovie initialised: " ++ (show mov)


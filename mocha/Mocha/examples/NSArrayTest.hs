{-# OPTIONS -fglasgow-exts #-}

module Main where

import Prelude hiding (init)

import Foreign.C
import Foreign.Ptr

import Mocha
import DirectMessagingExample

-- default (Integer, CUInt, Double)

main :: IO ()
main = withAutoreleasePool arrayTest

arrayTest = do
   o <- _NSURL_ # alloc () >>= initWithString "http://localhost/"
   a <- _NSArray_ # arrayWithObject o
   o' <- a # objectAtIndex (0 :: CUInt) :: IO NSURL
   urlData <- _NSString_ # stringWithContentsOfURL o'
   putStr urlData


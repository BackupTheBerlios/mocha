{-# OPTIONS -fffi #-}

module Mocha.AutoreleasePool
(
   withAutoreleasePool
)
where

withAutoreleasePool :: IO a -> IO a
withAutoreleasePool action = do
   cPushAutoreleasePool
   returnValue <- action
   cPopAutoreleasePool
   return returnValue


foreign import ccall safe "MochaBridge.h pushAutoreleasePool"
   cPushAutoreleasePool :: IO ()

foreign import ccall safe "MochaBridge.h popAutoreleasePool"
   cPopAutoreleasePool :: IO ()


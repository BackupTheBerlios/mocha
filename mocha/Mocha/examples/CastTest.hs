{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

module CastTest where

class Cast from to where
   cast :: Cast to from => from -> to

class Upcast sub super

class Upcast sub super => Downcast super sub

data NSArray = NSArray

data NSObject = NSObject

instance Cast NSArray NSObject where
   cast NSArray = NSObject


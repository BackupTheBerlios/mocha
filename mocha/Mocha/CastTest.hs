{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}

module CastTest where

class UpcastDowncast sub super where
   upcast :: sub -> super
   downcast :: super -> sub

data NSObject = NSObject deriving (Show, Eq)
instance UpcastDowncast NSObject NSObject where
   upcast = id
   downcast = id

data NSArray = NSArray deriving (Show, Eq)
instance UpcastDowncast NSArray NSArray where
   upcast = id
   downcast = id
instance UpcastDowncast NSArray NSObject where
   upcast NSArray = NSObject
   downcast NSObject = NSArray

data Upcast = Upcast
data Downcast = Downcast

class Cast a from to where
   cast :: from -> to

instance Cast a NSObject NSArray
instance Cast a NSArray NSObject


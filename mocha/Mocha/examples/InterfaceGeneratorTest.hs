{-# OPTIONS -fglasgow-exts #-}

module InterfaceGeneratorTest

where

import Foreign.Ptr

import Base
import InterfaceGenerator
import Messaging
import TypeEncodings


$(wholeHierarchyQ)

foo :: NSDictionary -> ID
foo x = upcast x :: ID


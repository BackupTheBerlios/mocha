{-# OPTIONS -fglasgow-exts #-}

module Main where

import Control.Monad
import Prelude hiding (init)
import System

import DirectMessagingExample
import Mocha

main :: IO ()
main = do
   args <- System.getArgs
   when (args == []) $ do
      putStrLn "usage: URLFetcher <url>";
      System.exitFailure
   let (arg1:_) = args
   withAutoreleasePool (getURL arg1)

getURL :: String -> IO ()
getURL urlAsString = do
   url <- _NSURL_ # urlWithString urlAsString
   urlData <- _NSString_ # stringWithContentsOfURL url
   putStr urlData


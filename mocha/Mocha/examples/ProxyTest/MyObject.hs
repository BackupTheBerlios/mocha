{-# OPTIONS -fglasgow-exts #-}

module MyObject where

import Char
import Data.IORef
import Foreign.C
import Foreign.Ptr
import Prelude hiding (init)
import System.IO

import DirectMessagingExample
import Mocha

$(mochaExportClass
   Interface
   { className = "MyObject"
   , superclassName = "NSObject"
   , instanceVariables = ["aTextField", "aMovieView", "anImageView"]
   , methods =
      [ Method "rot13:" [t| ID -> IO () |]
      , Method "loadImage:" [t| ID -> IO () |]
      , Method "loadMovie:" [t| ID -> IO () |]
      , Method "playOrPauseMovie:" [t| ID -> IO () |]
      ]
   }
 )

aTextField  = newInstanceVariable "aTextField" nil
aMovieView  = newInstanceVariable "aMovieView" nil
anImageView = newInstanceVariable "anImageView" nil

-- Implementation

rot13 :: ID -> IO ()
rot13 _ = do
   textField <- getInstanceObject aTextField :: IO NSTextField
   text <- textField # stringValue ()
   textField # setStringValue (map rot13C text)
   putStrLnErr "Haskell ROT13 executed"

rot13C :: Char -> Char
rot13C c
   | c `elem` ['a'..'m'] || c `elem` ['A'..'M'] = chr (ord c + 13)
   | c `elem` ['n'..'z'] || c `elem` ['N'..'Z'] = chr (ord c - 13)
   | otherwise = c

loadMovie :: ID -> IO ()
loadMovie _ = do
   url <- getUserInputAsURL
   mv <- getInstanceObject aMovieView :: IO NSMovieView
   theMovie <- _NSMovie_ # alloc () >>= initWithURL_byReference (url, True)
   mv # setMovie theMovie

loadImage :: ID -> IO ()
loadImage _ = do
   url <- getUserInputAsURL
   iv <- getInstanceObject anImageView :: IO NSImageView
   i <- _NSImage_ # alloc () >>= initByReferencingURL url
   iv # setImage i

getUserInputAsURL :: IO NSURL
getUserInputAsURL = do
   textField <- getInstanceObject aTextField :: IO NSTextField
   text <- textField # stringValue ()
   _NSURL_ # urlWithString text

playOrPauseMovie :: ID -> IO ()
playOrPauseMovie _ = do
   mv <- getInstanceObject aMovieView :: IO NSMovieView
   isMoviePlaying <- mv # isPlaying ()
   case isMoviePlaying of
      False -> mv # start nil
      True -> mv # stop nil

putStrLnErr = hPutStrLn stderr

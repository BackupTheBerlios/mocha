{-# OPTIONS -fglasgow-exts #-}

module DirectMessagingExample

where

import Foreign.C.Types
import Foreign.Ptr

import Mocha.Base
import Mocha.DirectMessaging
import Mocha.ExportTemplates
import Mocha.InterfaceGenerator
import Mocha.Messaging
import Mocha.TypeEncodings

-- FIXME: all of the stuff below should be automatically generated via TH

data ObjCMethodName_count

instance DirectMessage NSDictionary ObjCMethodName_count () Int
instance DirectMessage NSArray ObjCMethodName_count () Int
count :: (DirectMessage receiver ObjCMethodName_count arguments reply)
      => arguments -> receiver -> IO reply
count arguments receiver = sendMessage "count" arguments (cast receiver :: ID)


data ObjCMethodName_alloc

instance DirectMessage NSAutoreleasePoolClassObject ObjCMethodName_alloc () NSAutoreleasePool
instance DirectMessage NSDictionaryClassObject ObjCMethodName_alloc () NSDictionary
instance DirectMessage NSArrayClassObject ObjCMethodName_alloc () NSArray
instance DirectMessage NSURLClassObject ObjCMethodName_alloc () NSURL
instance DirectMessage NSImageClassObject ObjCMethodName_alloc () NSImage
instance DirectMessage NSMovieClassObject ObjCMethodName_alloc () NSMovie
instance DirectMessage HSProxyClassObject ObjCMethodName_alloc () HSProxy
instance DirectMessage NSTextFieldClassObject ObjCMethodName_alloc () NSTextField
alloc :: (DirectMessage receiver ObjCMethodName_alloc arguments reply)
      => arguments -> receiver -> IO reply
alloc arguments receiver = sendMessage "alloc" arguments (cast receiver :: ID)


data ObjCMethodName_init

instance DirectMessage NSAutoreleasePool ObjCMethodName_init () NSAutoreleasePool
instance DirectMessage NSDictionary ObjCMethodName_init () NSDictionary
instance DirectMessage NSArray ObjCMethodName_init () NSArray
instance DirectMessage HSProxy ObjCMethodName_init () HSProxy
instance DirectMessage NSTextField ObjCMethodName_init () NSTextField
init :: (DirectMessage receiver ObjCMethodName_init arguments reply)
     => arguments -> receiver -> IO reply
init arguments receiver = sendMessage "init" arguments (cast receiver :: ID)


data ObjCMethodName_initWithScheme_host_path
instance DirectMessage NSURL ObjCMethodName_initWithScheme_host_path (String, String, String) NSURL
initWithScheme_host_path :: (DirectMessage receiver ObjCMethodName_initWithScheme_host_path arguments reply)
                         => arguments -> receiver -> IO reply
initWithScheme_host_path arguments receiver = sendMessage "initWithScheme:host:path:" arguments (cast receiver :: ID)

data ObjCMethodName_setMovie_
instance DirectMessage NSMovieView ObjCMethodName_setMovie_ NSMovie ()
setMovie :: (DirectMessage receiver ObjCMethodName_setMovie_ arguments reply)
                         => arguments -> receiver -> IO reply
setMovie arguments receiver = sendMessage "setMovie:" arguments (cast receiver :: ID)

data ObjCMethodName_start_
instance (ObjCArguments o, ObjCObject o) => DirectMessage NSMovieView ObjCMethodName_start_ o ()
start :: (DirectMessage receiver ObjCMethodName_start_ arguments reply)
                         => arguments -> receiver -> IO reply
start arguments receiver = sendMessage "start:" arguments (cast receiver :: ID)

data ObjCMethodName_stop_
instance (ObjCArguments o, ObjCObject o) => DirectMessage NSMovieView ObjCMethodName_stop_ o ()
stop :: (DirectMessage receiver ObjCMethodName_stop_ arguments reply)
                         => arguments -> receiver -> IO reply
stop arguments receiver = sendMessage "stop:" arguments (cast receiver :: ID)

data ObjCMethodName_isPlaying
instance DirectMessage NSMovieView ObjCMethodName_isPlaying () Bool
isPlaying :: (DirectMessage receiver ObjCMethodName_isPlaying arguments reply)
                         => arguments -> receiver -> IO reply
isPlaying arguments receiver = sendMessage "isPlaying" arguments (cast receiver :: ID)

data ObjCMethodName_URLWithString_
instance DirectMessage NSURLClassObject ObjCMethodName_URLWithString_ String NSURL
urlWithString :: (DirectMessage receiver ObjCMethodName_URLWithString_ arguments reply)
                         => arguments -> receiver -> IO reply
urlWithString arguments receiver = sendMessage "URLWithString:" arguments (cast receiver :: ID)

data ObjCMethodName_initWithURL_byReference
instance DirectMessage NSMovie ObjCMethodName_initWithURL_byReference (NSURL, Bool) 
   NSMovie
initWithURL_byReference :: (DirectMessage receiver ObjCMethodName_initWithURL_byReference arguments reply)
                         => arguments -> receiver -> IO reply
initWithURL_byReference arguments receiver = sendMessage "initWithURL:byReference:" arguments (cast receiver :: ID)


data ObjCMethodName_initWithString_

instance DirectMessage
   NSURL
   ObjCMethodName_initWithString_
   String
   NSURL
initWithString :: (DirectMessage receiver ObjCMethodName_initWithString_ arguments reply)
                         => arguments -> receiver -> IO reply
initWithString arguments receiver = sendMessage "initWithString:" arguments (cast receiver :: ID)


data ObjCMethodName_absoluteString

instance DirectMessage NSURL ObjCMethodName_absoluteString () String
absoluteString :: (DirectMessage receiver ObjCMethodName_absoluteString arguments reply)
               => arguments -> receiver -> IO reply
absoluteString arguments receiver = sendMessage "absoluteString" arguments (cast receiver :: ID)


data ObjCMethodName_selector
instance DirectMessage
    NSInvocation
    ObjCMethodName_selector
    ()
    Selector
selector :: (DirectMessage receiver ObjCMethodName_selector arguments reply)
               => arguments -> receiver -> IO reply
selector arguments receiver = sendMessage "selector" arguments (cast receiver :: ID)

data ObjCMethodName_stringValue
instance DirectMessage
   NSTextField
   ObjCMethodName_stringValue
   ()
   String
stringValue :: (DirectMessage receiver ObjCMethodName_stringValue arguments reply)
               => arguments -> receiver -> IO reply
stringValue arguments receiver = sendMessage "stringValue" arguments (cast receiver :: ID)

data ObjCMethodName_setStringValue_
instance DirectMessage
   NSTextField
   ObjCMethodName_setStringValue_
   String
   ()
setStringValue :: (DirectMessage receiver ObjCMethodName_setStringValue_ arguments reply)
               => arguments -> receiver -> IO reply
setStringValue arguments receiver = sendMessage "setStringValue:" arguments (cast receiver :: ID)

data ObjCMethodName_stringWithContentsOfURL_
instance DirectMessage
   NSStringClassObject
   ObjCMethodName_stringWithContentsOfURL_
   NSURL
   String
stringWithContentsOfURL :: (DirectMessage receiver ObjCMethodName_stringWithContentsOfURL_ arguments reply)
               => arguments -> receiver -> IO reply
stringWithContentsOfURL arguments receiver = sendMessage "stringWithContentsOfURL:" arguments (cast receiver :: ID)

data ObjCMethodName_release
instance DirectMessage
   NSAutoreleasePool
   ObjCMethodName_release
   ()
   ()
release :: (DirectMessage receiver ObjCMethodName_release arguments reply)
        => arguments -> receiver -> IO reply
release arguments receiver = sendMessage "release" arguments (cast receiver :: ID)

data ObjCMethodName_initByReferencingURL_
instance DirectMessage NSImage ObjCMethodName_initByReferencingURL_ NSURL NSImage
initByReferencingURL :: (DirectMessage receiver ObjCMethodName_initByReferencingURL_ arguments reply)
                     => arguments -> receiver -> IO reply
initByReferencingURL arguments receiver = sendMessage "initByReferencingURL:" arguments (cast receiver :: ID)

data ObjCMethodName_setImage_
instance DirectMessage NSImageView ObjCMethodName_setImage_ NSImage ()
setImage :: (DirectMessage receiver ObjCMethodName_setImage_ arguments reply)
         => arguments -> receiver -> IO reply
setImage arguments receiver = sendMessage "setImage:" arguments (cast receiver :: ID)

data ObjCMethodName_objectAtIndex_
instance DirectMessageAReply NSArray ObjCMethodName_objectAtIndex_ CUInt
objectAtIndex :: (ObjCMessageReply reply, DirectMessageAReply receiver ObjCMethodName_objectAtIndex_ arguments)
              => arguments -> receiver -> IO reply
objectAtIndex arguments receiver = sendMessage "objectAtIndex:" arguments (cast receiver :: ID)

{-
data ObjCMethodName_objectAtIndex_
instance DirectMessageBReply NSArray ObjCMethodName_objectAtIndex_ CUInt ID
instance DirectMessageBReply NSArray ObjCMethodName_objectAtIndex_ CUInt NSURL
objectAtIndex :: (DirectMessageBReply receiver ObjCMethodName_objectAtIndex_ arguments reply)
              => arguments -> receiver -> IO reply
objectAtIndex arguments receiver = sendMessage "objectAtIndex:" arguments (cast receiver :: ID)
-}

data ObjCMethodName_arrayWithObject_
instance ObjCArguments o => DirectMessage NSArrayClassObject ObjCMethodName_arrayWithObject_ o NSArray
arrayWithObject :: (DirectMessage receiver ObjCMethodName_arrayWithObject_ arguments reply)
	        => arguments -> receiver -> IO reply
arrayWithObject arguments receiver = sendMessage "arrayWithObject:" arguments (cast receiver :: ID)


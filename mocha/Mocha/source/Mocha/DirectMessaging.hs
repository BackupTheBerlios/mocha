{-# OPTIONS -fglasgow-exts #-}

module Mocha.DirectMessaging 

where

import Foreign.C.String
import Foreign.Ptr

import Mocha.AutoMarshal
import Mocha.Base
import Mocha.Messaging

class (ObjCArguments arguments,
       ObjCObject receiver,
       ObjCMessageReply reply) =>
       DirectMessage receiver method arguments reply
          | receiver method arguments -> reply

class (ObjCArguments arguments,
       ObjCObject receiver) =>
       DirectMessageAReply receiver method arguments

class (ObjCArguments arguments,
       ObjCObject receiver,
       ObjCMessageReply reply) =>
       DirectMessageBReply receiver method arguments reply


module Main() 
    where

import System.IO

-- import System.Log.Logger
-- import System.Log.Handler.Simple

import Server
import MessageHandler

import Crypto
import Message
import Codec.Utils
import Codec.Binary.Base64.String

msg = Message (ClientId "alex") "digest" "text"

convert x = (encode . toChars . toOctets 256) x

open_key = (x, y) 
           where x = convert 3
                 y = convert 60
                 
private_key = (x, y) 
           where x = convert 13
                 y = convert 60

main = do
  let em = encodeMessage open_key msg
  let dm = decodeMessage private_key em

  print $ open_key
  print $ private_key
  
  print $ em
  print $ dm
  
-- runServer handleMessage


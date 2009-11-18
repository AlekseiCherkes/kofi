module Crypto 
       where 

import Message
import Codec.Utils
import Data.Digest.MD5
import Codec.Encryption.RSA
import Codec.Binary.Base64.String

--------------------------------------------------------------------------------
-- Common data types
--------------------------------------------------------------------------------

type RSAKey = (Base64, Base64)
type EncodedMessage = Base64

createMessage :: RSAKey -> SenderId -> String -> Message
createMessage key senderId text = Message { senderId = senderId, 
                                            digest = digest,
                                            text = text }
  where digest = encode $ toChars $ hash $ toOctetsMy text
        

encodeMessage :: RSAKey -> Message -> EncodedMessage
encodeMessage key msg = encode $ toChars $ encrypt dkey dmsg
  where
    dmsg = toOctetsMy (show msg)
    dkey = (cast (fst key), cast (snd key))
           where cast x = (toOctetsMy . decode) x
      


decodeMessage :: RSAKey -> EncodedMessage -> Message
decodeMessage key msg = (read msg )::Message

-- verifyMessage :: Message -> RSAKey -> Bool

--------------------------------------------------------------------------------
-- Utilitis
--------------------------------------------------------------------------------

toOctetsMy :: [Char] -> [Octet]
toOctetsMy cs = map (toEnum . fromEnum) cs

toChars :: [Octet] -> [Char]
toChars os = map (toEnum . fromEnum) os

--------------------------------------------------------------------------------

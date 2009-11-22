module Crypto 
       where 

import Debug.Trace 

import Message
import Data.Word
import Codec.Utils
import qualified Data.Digest.MD5 as MD5
import qualified Codec.Encryption.RSA as RSA
import qualified Codec.Binary.Base64.String as B64

--------------------------------------------------------------------------------
-- Common data types
--------------------------------------------------------------------------------

type RSAKey = (Base64, Base64) -- (n, e)
type EncryptedMessage = Base64

-- createMessage :: RSAKey -> SenderId -> String -> Message
-- createMessage key senderId text = Message { senderId = senderId, 
--                                             digest = digest,
--                                             text = text }
--   where digest = B64.encode $ toChars $ MD5.hash $ toOctetsMy text

encodeMessage :: RSAKey -> String -> EncryptedMessage
encodeMessage key msg = (--B64.encode . 
                         octetsToChar8 . 
                         groupMap 1 (RSA.encrypt dkey) . 
                         char16ToOctets) msg
  where dkey = b64ToKey key
        keyLen = length $ fst dkey

decodeMessage :: RSAKey -> EncryptedMessage -> String -- Message
decodeMessage key msg = dmsg -- read dmsg
  where dmsg = (octetsToChar16 .
                --groupMap 2 (RSA.decrypt dkey) .
                char8ToOctets)
                --B64.decode) 
               msg
        dkey = b64ToKey key
        keyLen = length $ fst dkey

-- verifyMessage :: Message -> RSAKey -> Bool

--------------------------------------------------------------------------------
-- Utilitis
--------------------------------------------------------------------------------

groupMap n f xs | n == 0 = error "groupMap: n == 0"
                | length xs >= n = f (take n xs) ++ groupMap n f (drop n xs)
                | length xs == 0 = []
                | otherwise = f xs

octetsToChar8 os = map wordToChar os
  where wordToChar = (toEnum . fromEnum) :: Word8 -> Char
                   
char8ToOctets cs = map charToWord cs
  where charToWord = (toEnum . fromEnum) :: Char -> Word8

-- !!!
char16ToOctets :: String -> [Octet]
char16ToOctets cs = listToOctets $ map charToWord cs
  where charToWord = (toEnum . fromEnum) :: Char -> Word16
                                             
-- !!!
octetsToChar16 :: [Octet] -> String
octetsToChar16 os = map wordToChar (listFromOctets os)
  where wordToChar = (toEnum . fromEnum) :: Word16 -> Char
        
keyToB64 :: ([Octet], [Octet]) -> RSAKey
keyToB64 (n, e) = (convert n, convert e)
  where convert = B64.encode . octetsToChar8
        
b64ToKey :: RSAKey -> ([Octet], [Octet])
b64ToKey (n, e) = (convert n, convert e)
  where convert = char8ToOctets . B64.decode

--------------------------------------------------------------------------------

module Main
       where
       
import Types
import Message
import Crypto

import Network
import System.IO

--------------------------------------------------------------------------------
-- Messages
--------------------------------------------------------------------------------

sendRSAKey = ("MI8=","DQ==")
recvRSAKey = ("R2s=","BV0=")
servRecvRSAKey = ("MI8=","K0U=")

myUNP = str2unp "7011293625508"
apk1 = AccountPK (str2acc "6801954585387") (str2bic "151501267")
apk2 = AccountPK (str2acc "8519545454900") (str2bic "153001266")

testTransaction = CommitedTransaction { reason = "test this client server communication"
                                      , creditAccount = apk1
                                      , debitAccount  = apk2
                                      , amount = 100.0
                                      , priority = Normal
                                      }

msg_body = GetBalance apk2
-- body = CommitedTransaction testTransaction

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main = withSocketsDo $ do
  let mb = (show msg_body)  
  let emb = encodeMessageBody sendRSAKey mb
  let msg = createMessage sendRSAKey (ClientId myUNP) emb
  let dmb = decodeMessageBody servRecvRSAKey emb      
      
  print $ "Message body: " ++ mb
  print $ "Encrypted message body: " ++ emb
  print $ "Decrypted message body: " ++ dmb
  testSendAndRecv msg

--------------------------------------------------------------------------------
-- Network utilits
--------------------------------------------------------------------------------

host = "127.0.0.1"
port = PortNumber 6555

testSendAndRecv message = do
  h <- connectTo host port
  hPutStrLn h (show message)
  hFlush h
  ret <- hGetLine h
  hClose h
  print $ "Response: " ++ ret
  let msg = read ret :: Message
  print $ "Msg: " ++ (show msg)
  let dmb = decodeMessageBody recvRSAKey $ body msg
  print $ "Decoded response: " ++ dmb

--------------------------------------------------------------------------------
-- End
--------------------------------------------------------------------------------

-- module Main
--     where

-- import System.IO

-- -- import System.Log.Logger
-- -- import System.Log.Handler.Simple

-- import Crypto
-- import Message
-- import Codec.Utils
-- import qualified Codec.Binary.Base64.String as B64

-- euclid a 0 = (a, (1, 0))
-- euclid a b = (d', (y', x' - (a `div` b) * y'))
--     where
--       ret = euclid b (a `mod` b)
--       d'  = fst ret
--       x'  = fst $ snd ret
--       y'  = snd $ snd ret

-- -- modular liner equation solver
      
-- data MlesReturn a = BadParams | NoRoots | Result [a]
--                    deriving (Show)
                            
-- myResult (Result a) = a

-- mles a b n | a <= 0 || n <= 0 = BadParams
--            | b `mod` d == 0 = let x0 = ((x' * (b `div` d)) `mod` n) in
--                               Result [(x0 + i * (n `div` d)) `mod` n | i <- [0 .. d - 1]]
--            | otherwise = NoRoots
--            where ret = euclid a n
--                  d   = fst ret
--                  x'  = fst $ snd ret
--                  y'  = snd $ snd ret
                 
-- --------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------

-- p = 17
-- q = 19
-- -- p = 337
-- -- q = 263
-- n = p * q
-- fn = (p - 1) * (q - 1)
-- e = 7
-- d = head $ myResult (mles e 1 fn)

-- cast n e = keyToB64 $ (toOctets 256 n, toOctets 256 e)

-- open_key = ("MI8=","K0U=")
-- private_key = ("MI8=","DQ==")

-- -- open_key = cast n e
-- -- private_key = cast n d

-- mb = "asdfфылдвор"
-- em = encodeMessageBody open_key mb
-- dm = decodeMessageBody private_key em

-- --------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------

-- main = do
--   print $ (n, e)
--   print $ (n, d)
--   print $ open_key
--   print $ private_key
--   -- print $ (fromOctets 256 . b64ToKey) $ fst open_key
--   -- print $ private_key
  
--   print $ mb
--   print $ em
--   print $ dm
  
-- -- runServer handleMessage
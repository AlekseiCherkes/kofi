<<<<<<< HEAD:Server/Main.hs
module Main
    where

import System.IO

-- import System.Log.Logger
-- import System.Log.Handler.Simple

=======
>>>>>>> hdc/master:Src/Server/Main.hs
import Server
import MessageHandler

import Debug.Trace

import Crypto
import Message
import Codec.Utils
import qualified Codec.Binary.Base64.String as B64

euclid a 0 = (a, (1, 0))
euclid a b = (d', (y', x' - (a `div` b) * y'))
    where
      ret = euclid b (a `mod` b)
      d'  = fst ret
      x'  = fst $ snd ret
      y'  = snd $ snd ret

-- modular liner equation solver
      
data MlesReturn a = BadParams | NoRoots | Result [a]
                   deriving (Show)
                            
myResult (Result a) = a

mles a b n | a <= 0 || n <= 0 = BadParams
           | b `mod` d == 0 = let x0 = ((x' * (b `div` d)) `mod` n) in
                              Result [(x0 + i * (n `div` d)) `mod` n | i <- [0 .. d - 1]]
           | otherwise = NoRoots
           where ret = euclid a n
                 d   = fst ret
                 x'  = fst $ snd ret
                 y'  = snd $ snd ret
                 
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

p = 17
q = 19
-- p = 337
-- q = 263
n = p * q
fn = (p - 1) * (q - 1)
e = 7
d = head $ myResult (mles e 1 fn)

cast n e = keyToB64 $ (toOctets 256 n, toOctets 256 e)

open_key = cast n e
private_key = cast n d

-- msg = Message (ClientId "alex") "2134089" "text текст фыжва..юбыь!!-09"
msg = createMessage open_key (ClientId "123") "text яяя !! жыдлвоафылваожлд.юб345щ2347590879087"

em = encodeMessage open_key msg
dm = decodeMessage private_key em

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

main = do
  print $ (n, e)
  print $ (n, d)
  print $ open_key
  print $ private_key
  -- print $ (fromOctets 256 . b64ToKey) $ fst open_key
  -- print $ private_key
  
  print $ msg
  print $ verifyMessage private_key msg
--  print $ em
--  print $ dm
  
-- runServer handleMessage


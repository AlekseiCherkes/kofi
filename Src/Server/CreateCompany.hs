module Main()
       where 

import WithDB
import Crypto
import Queries
import ClientConfig
import Entity

import System.IO
import System.Environment
import System.Random
import System.Time
import Codec.Encryption.RSA.NumberTheory
import Codec.Utils
import qualified Codec.Binary.Base64.String as B64

--------------------------------------------------------------------------------
-- Helper functions from number theory
--------------------------------------------------------------------------------

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
-- Helper functions
--------------------------------------------------------------------------------

parseArgs args | length args /= 1 = error "Invalid keys.\n create_company <company name>"
               | otherwise = head $ args
                             
rand min max = getStdRandom $ randomR (min, max)
                             
generateUnp = do
  unp <- rand min max
  return (show $ unp)
  where 
    min = 10^8::Integer
    max = 10^9 - 1::Integer

generateKeyPair r1 r2 r3 = (open_key, private_key)  
  where 
    p = primes !! r1
    q = primes !! r2
    e = primes !! r3
    n = p * q
    fn = (p - 1) * (q - 1)
    d = head $ myResult (mles e 1 fn)
    cast n e = keyToB64 $ (toOctets 256 n, toOctets 256 e)
    open_key = cast n e
    private_key = cast n d
    
--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------
                  
main = do
  args <- getArgs
  let name = parseArgs args
  print $ "Create company: " ++ name
  
  r1 <- rand 100 1000
  r2 <- rand 100 1000
  r3 <- rand 10 (100 - 1)

  unp <- generateUnp
  date <- getClockTime >>= toCalendarTime
  let (publicKey, privateKey) = generateKeyPair r1 r2 r3
      
  print $ "UNP: " ++ (show $ unp)
  print $ "Time: " ++ (show $ date)
  print $ "Keys: " ++ (show $ publicKey) ++ "; " ++ (show $ privateKey)
  
  let cmp = Company { unp = unp 
                    , name = name 
                    , registryDate = date
                    , unregistryDate = Nothing
                    , openKey = show publicKey
                    }
            
  withDB $ insertCompany cmp

  let cc = ClientConfig { unp = unp 
                        , name = name 
                        , registryDate = date
                        , privateKey = privateKey
                        }
           
  let fileName = unp ++ ".config"           
  withFile fileName WriteMode (\h -> hPrint h (show cc))

--------------------------------------------------------------------------------
  
  





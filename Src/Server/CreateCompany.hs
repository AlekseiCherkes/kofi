module Main()
       where 

import Crypto
import DataModel

import System.IO
import System.Environment
import System.Random
import System.Time
import Codec.Encryption.RSA.NumberTheory
import Codec.Utils
import qualified Codec.Binary.Base64.String as B64

import Database.HSQL
import System.Exit

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
myResult NoRoots = error "NoRoots"
myResult BadParams = error "BadParams"

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

parseArgs args | length args /= 1 = error "Invalid command line params."
               | otherwise = head $ args
                             
rand min max = getStdRandom $ randomR (min, max)
                             
generateUnp = do
  unp <- rand min max
  return (show $ unp)
  where 
    min = 10^8::Integer
    max = 10^9 - 1::Integer
    
findNearestPrime r fn | fn `mod` p == 0 = findNearestPrime (r + 1) fn
                      | otherwise = p
                        where p = (primes !! r)

generateKeyPair r1 r2 r3 = (open_key, private_key)  
  where 
    p = primes !! r1
    q = primes !! r2
    n = p * q
    fn = (p - 1) * (q - 1)
    e = findNearestPrime r3 fn
    d = head $ myResult (mles e 1 fn)
    cast n e = keyToB64 $ (toOctets 256 n, toOctets 256 e)
    open_key = cast n e
    private_key = cast n d
    
--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------
                  
main = do
  name <- (getArgs >>= \as -> return (parseArgs as))
  setStdGen $ read (take 6 name) -- use company name for generate UNP
  
  r1 <- rand 100 1000
  r2 <- rand 100 1000
  r3 <- rand 10 (100 - 1)
  
  r4 <- rand 100 1000
  r5 <- rand 100 1000
  r6 <- rand 10 (100 - 1)

  unp <- generateUnp
  date <- getClockTime >>= toCalendarTime
  let (clientSendKey, serverRecvKey) = generateKeyPair r1 r2 r3
  let (serverSendKey, clientRecvKey) = generateKeyPair r4 r5 r6
      
  let cmp = Company { unp = unp
                    , name = name
                    , registryDate = date
                    , unregistryDate = Nothing
                    , serverRecvKey = serverRecvKey
                    , serverSendKey = serverSendKey
                    , clientRecvKey = clientRecvKey
                    , clientSendKey = clientSendKey
                    }

  catchSql 
    (withDB $ \conn -> insertCompany conn cmp)
    (\e -> (print $ show e) >> exitFailure)
  
  print unp
  print name
  print date
  print clientRecvKey
  print clientSendKey

--------------------------------------------------------------------------------
-- End
--------------------------------------------------------------------------------
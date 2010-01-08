module Main()
       where

import DataModel
import Loggers
import Types

import System.Environment
import System.Time
import System.Random
import System.Exit
import Data.Bits
import Data.Word

import Prelude hiding (print)
import System.IO.UTF8
import qualified Codec.Binary.UTF8.String as UTF8

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

parseArgs :: [String] -> (String, String, Double)
parseArgs args | length args /= 3 = error "Invalid command line params."
               | otherwise = (UTF8.decodeString $ args !! 0, 
                              UTF8.decodeString $ args !! 1, 
                              read $ UTF8.decodeString $ args !! 2)

rand min max = getStdRandom $ randomR (min, max)

generateAccId = do
  aid <- rand min max
  return $ str2acc (show $ aid)
  where 
    min = 10^12::Integer
    max = 10^13 - 1::Integer

hashString :: String -> Int
hashString str = (toEnum . fromEnum) $ 
                 foldl1 xor $ 
                 ((map (toEnum . fromEnum) str) :: [Word16])

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

main = withUtilityLoggers $ \_ -> do
  args <- getArgs
  let (unp, bic, ballance) = parseArgs args
      
  setStdGen $ mkStdGen $ hashString $ foldl1 (++) args
  aid <- generateAccId
  date <- getClockTime >>= toCalendarTime
                         
  let account = Account (AccountPK aid (str2bic bic))
                (str2unp unp) ballance date Nothing

  insertAccount account
  
  print $ acc2str aid

--------------------------------------------------------------------------------
-- End
--------------------------------------------------------------------------------

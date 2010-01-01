module Main()
       where

import DataModel
import Loggers

import System.Environment
import System.Time
import System.Random
import System.Exit
import Data.Bits
import Data.Word

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

parseArgs :: [String] -> (String, String, Double)
parseArgs args | length args /= 3 = error "Invalid command line params."
               | otherwise = (args !! 0, args !! 1, read (args !! 2))

rand min max = getStdRandom $ randomR (min, max)

generateAccId = do
  aid <- rand min max
  return (show $ aid)
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
                         
  let account = Account aid bic unp ballance date Nothing

  insertAccount account
  
  print aid

--------------------------------------------------------------------------------
-- End
--------------------------------------------------------------------------------

module System.Environment.UTF8
  (getArgs, getProgName, getEnv, withArgs, withProgName, getEnvironment)
  where

import Codec.Binary.UTF8.String (decodeString)
import qualified System.Environment as Sys

getArgs :: IO [String]
getArgs = map decodeString `fmap` Sys.getArgs

getProgName :: IO String
getProgName = decodeString `fmap` Sys.getProgName

getEnv :: String -> IO String
getEnv x = decodeString `fmap` Sys.getEnv x

withArgs :: [String] -> IO a -> IO a
withArgs = Sys.withArgs

withProgName :: String -> IO a -> IO a
withProgName = Sys.withProgName

getEnvironment :: IO [(String,String)]
getEnvironment = map f `fmap` Sys.getEnvironment
  where f (a,b) = (decodeString a, decodeString b)

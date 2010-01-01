module Loggers
       where

import System.IO
import System.Directory
import Control.Exception
import System.Log.Logger
import System.Log.Handler.Simple
import qualified System.Log.Handler

withServerLoggers = bracket (acquireServerLoggers "server") releaseLoggers
withUtilityLoggers = bracket (acquireUtilityLoggers "uility") releaseLoggers

logDir = "log/"
logFileHandler path = fileHandler (logDir ++ path)

acquireServerLoggers appName = do
  createDirectoryIfMissing True logDir
  
  stderrH <- verboseStreamHandler stderr INFO
  h <- logFileHandler (appName ++ "-root") DEBUG
  dbH <- logFileHandler (appName ++ "-root-db") DEBUG
  serverH <- logFileHandler (appName ++ "-root-server") DEBUG
  clientsH <- logFileHandler (appName ++ "-root-client") DEBUG
  tellerH <- logFileHandler (appName ++ "-root-teller") DEBUG
  
  updateGlobalLogger (rootLoggerName::String) (setLevel DEBUG . setHandlers [stderrH])
  updateGlobalLogger "root" (setLevel DEBUG . setHandlers [h])
  updateGlobalLogger "root.db" (setLevel DEBUG . setHandlers [dbH])
  updateGlobalLogger "root.server" (setLevel DEBUG . setHandlers [serverH])
  updateGlobalLogger "root.client" (setLevel DEBUG . setHandlers [clientsH])
  updateGlobalLogger "root.teller" (setLevel DEBUG . setHandlers [tellerH])
  
  return [h, dbH, serverH, clientsH, tellerH]

acquireUtilityLoggers appName = do
  createDirectoryIfMissing True logDir
  
  stderrH <- verboseStreamHandler stderr INFO
  h <- logFileHandler (appName ++ "-root") DEBUG
  dbH <- logFileHandler (appName ++ "-root-db") DEBUG
  
  updateGlobalLogger (rootLoggerName::String) (setLevel DEBUG . setHandlers [stderrH])
  updateGlobalLogger "root" (setLevel DEBUG . setHandlers [h])
  updateGlobalLogger "root.db" (setLevel DEBUG . setHandlers [dbH])
  
  return [h, dbH]


releaseLoggers loggers = mapM (System.Log.Handler.close) loggers
  


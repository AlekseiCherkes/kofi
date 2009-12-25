module Loggers(withLoggers)
       where

import System.IO
import System.Directory
import Control.Exception
import System.Log.Logger
import System.Log.Handler.Simple
import qualified System.Log.Handler

withLoggers = bracket acquireLoggers releaseLoggers

logDir = "log/"

acquireLoggers = do
  stderrH <- verboseStreamHandler stderr INFO
  
  createDirectoryIfMissing True logDir
  let logFileHandler path = fileHandler (logDir ++ path)
      
  h <- logFileHandler "server" DEBUG
  dbH <- logFileHandler "server-db" DEBUG
  serverH <- logFileHandler "server-server" DEBUG
  clientsH <- logFileHandler "server-client" DEBUG
  tellerH <- logFileHandler "server-teller" DEBUG
  
  updateGlobalLogger (rootLoggerName::String) (setLevel DEBUG . setHandlers [stderrH])
  updateGlobalLogger "server" (setLevel DEBUG . setHandlers [h])
  updateGlobalLogger "server.db" (setLevel DEBUG . setHandlers [dbH])
  updateGlobalLogger "server.server" (setLevel DEBUG . setHandlers [serverH])
  updateGlobalLogger "server.client" (setLevel DEBUG . setHandlers [clientsH])
  updateGlobalLogger "server.teller" (setLevel DEBUG . setHandlers [tellerH])
  
  return [h, dbH, serverH, clientsH]
  
releaseLoggers loggers = mapM (System.Log.Handler.close) loggers
  


module Loggers(withLoggers)
       where

import System.IO
import Control.Exception
import System.Log.Logger
import System.Log.Handler.Simple
import qualified System.Log.Handler

withLoggers = bracket acquireLoggers releaseLoggers

acquireLoggers = do 
  stderrH <- verboseStreamHandler stderr INFO
  -- stdoutH <- verboseStreamHandler stdout DEBUG
  h <- fileHandler "server.log" DEBUG
  dbH <- fileHandler "server.db.log" DEBUG
  serverH <- fileHandler "server.server.log" DEBUG
  clientsH <- fileHandler "server.clients.log" DEBUG
  
  updateGlobalLogger (rootLoggerName::String) (setLevel DEBUG . setHandlers [stderrH])
  updateGlobalLogger "server" (setLevel DEBUG . setHandlers [h])
  updateGlobalLogger "server.db" (setLevel DEBUG . setHandlers [dbH])
  updateGlobalLogger "server.server" (setLevel DEBUG . setHandlers [serverH])
  updateGlobalLogger "server.clients" (setLevel DEBUG . setHandlers [clientsH])
  return [h, dbH, serverH, clientsH]
  
releaseLoggers loggers = mapM (System.Log.Handler.close) loggers
  


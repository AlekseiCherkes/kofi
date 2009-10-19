module Main() 
    where

import System.IO

import System.Log.Logger
import System.Log.Handler.Simple

import Listener
import MessageHandler

main = do
  stderrh <- verboseStreamHandler stderr INFO
  fileh <- fileHandler "server.log"  DEBUG
  updateGlobalLogger "server" (setLevel DEBUG . setHandlers [stderrh, fileh])
  infoM "server" "Starting server main."
  listen handleMessage


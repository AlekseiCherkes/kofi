module Main
       where
       
import Teller
import Server
import Loggers
import MessageHandler

import Control.Concurrent
import Control.Concurrent.Chan

main = withServerLoggers $ \_ -> do
  urgents <- newChan
  normals <- newChan
  forkIO $ startTeller urgents "urgents" (20 * 10^6)
  forkIO $ startTeller normals "normals" (50 * 10^6) 
  runServer handleMessage
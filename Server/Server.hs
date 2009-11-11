module Server(runServer)
    where

import Network
import System.IO
import Control.Monad

import Control.Concurrent
import Control.Exception

port = PortNumber 6555

runServer :: (String -> Handle -> IO ()) -> IO ()
runServer connHandler = withSocketsDo $ do
  withListenSock port (\sock -> do
                          forever $ do
                            (handle, name, port) <- accept sock
                            forkIO $ connHandler name handle
                      )
               
withListenSock port = bracket (listenOn port) (sClose)


-- serviceConn :: Socket -> (String -> String -> IO ()) -> IO ()
-- serviceConn sock f = do
--   (handle, name, port) <- accept sock
--   msg <- hGetLine handle
--   f name msg
--   loop sock f
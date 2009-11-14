module Server(runServer)
    where

import Prelude hiding (catch)
import Network
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Exception

port = PortNumber 6555

runServer :: (String -> Handle -> IO()) -> IO ()
runServer connHandler = 
  run `catch` handleException
  where 
    run = withSocketsDo $ do
      withListenSock listenLoop
      where
        withListenSock = bracket (listenOn port) (sClose)
        listenLoop sock = forever $ do
          (handle, name, client_port) <- accept sock
          serviceConn name handle connHandler
    
    handleException (e::SomeException) = do
      print $ "Common server error: " ++ (show e)    

serviceConn :: String -> Handle -> (String -> Handle -> IO ()) -> IO ()
serviceConn name handle connHandler = do
  threadId <- forkIO $ (connHandler name handle `catch` handleException)
  return ()
  where
    handleException (e::SomeException) = do
      print $ "Common error in client thread: " ++ (show e)
      hClose handle

module Server(runServer)
    where

import Prelude hiding (catch)
import Network
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Exception
import System.Log.Logger

port = PortNumber 6555

runServer :: (String -> Handle -> IO()) -> IO ()
runServer connHandler = do
  infoM "root.server" $ "Run server" -- " -- ++ "port = " ++ (show port)
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
      emergencyM "root.server" $ "Exception in main thread: " ++ (show e)

serviceConn :: String -> Handle -> (String -> Handle -> IO ()) -> IO ()
serviceConn name handle connHandler = do
  infoM "root.server" $ "Accept connection: " ++ name
  threadId <- forkIO $ connHandler name handle `catch` handleException
  return ()
  where
    handleException (e::SomeException) = do
      errorM "root.server" $ "Exception in client thread( " ++ name ++ " ): " ++ (show e)
      hClose handle

module Server(runServer)
    where

import Data.Maybe
import Prelude hiding (catch)
import Network
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Exception
import qualified System.Log.Logger as Logger

--------------------------------------------------------------------------------
-- Logging utility functions
--------------------------------------------------------------------------------

infoM = Logger.infoM "root.server"
errorM = Logger.errorM "root.server"

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

port = PortNumber 6555

--------------------------------------------------------------------------------
-- Server
--------------------------------------------------------------------------------

runServer connHandler = do
  infoM $ "Run server." -- ++ "port = " ++ (show $ (toEnum port)::Int)
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
      errorM $ "Exception in main thread: " ++ (show e)

serviceConn name handle connHandler = do
  infoM $ "Accept connection: " ++ name
  infoM $ "Starting client thread."
  threadId <- forkIO $ (perform handle) `catch` handleException
  return ()
  where
    perform h = do
      cnts <- hGetLine h
      infoM $ "Request contents: " ++ cnts
      resp <- connHandler cnts
      if (isJust resp)
        then do
          infoM $ "Response contents: " ++ (fromJust resp)
          hPutStrLn h (fromJust resp)
          hFlush h
        else 
          infoM $ "No response."
      -- hClose h
    handleException (e::SomeException) = do
      errorM $ "Exception in client thread( " ++ name ++ " ): " ++ (show e)
      hClose handle

--------------------------------------------------------------------------------
-- End
--------------------------------------------------------------------------------

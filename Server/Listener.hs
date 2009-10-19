module Listener(listen)
    where

import Network
import System.IO

port = PortNumber 6555

listen :: (String -> String -> IO ()) -> IO ()
listen f = withSocketsDo $ do
             sock <- listenOn port
             loop sock f
             sClose sock -- ???

loop :: Socket -> (String -> String -> IO ()) -> IO ()
loop sock f = do
  (handle, name, port) <- accept sock
  msg <- hGetLine handle
  f name msg
  loop sock f
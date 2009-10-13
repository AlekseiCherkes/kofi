module Main() 
    where

import Network
import System.IO

port = PortNumber 6555

main = withSocketsDo $ do
         sock <- listenOn port
         loop sock
         sClose sock

loop :: Socket -> IO ()
loop sock = do
  client <- accept sock
  withClient (snd3 client) (fst3 client)
  loop sock
    where
      fst3 (a, _, _) = a
      snd3 (_, b, _) = b
      tht3 (_, _, c) = c

withClient :: HostName -> Handle -> IO ()
withClient name handle = do
  print $ "Message from: " ++ name
  msg <- hGetLine handle
  print msg
  hClose handle
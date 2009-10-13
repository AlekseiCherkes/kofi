module Main()
    where

import Network
import System.IO

host = "127.0.0.1"
port = PortNumber 6555

main =  withSocketsDo $ do
          h <- connectTo host port
          hPrint h "Hello, server!"
          hClose h
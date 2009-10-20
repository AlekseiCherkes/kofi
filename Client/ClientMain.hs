module Main()
    where

import Network
import System.IO

import Message
import Entity

host = "127.0.0.1"
port = PortNumber 6555

testRequest = GetBalance

testMsg = Message { unp = 123456789
                  , body = show testRequest
                  , digest = 0
                  }

main = withSocketsDo $ do
         h <- connectTo host port
         hPrint h (show testMsg)
         hClose h
module Main()
    where

import Network
import System.IO

import Message

host = "127.0.0.1"
port = PortNumber 6555

testTransaction = CommitedTransaction { reason = "test this client server communication"
                                      , creditAccountId = 123456789
                                      , debitAccountId = 987654321
                                      , amount = 100.0
                                      , priority = Urgent
                                      }
                  
testRequest = CommitTransaction testTransaction


testMsg = Message { unp = 123456789
                  , body = show testRequest
                  , digest = 0
                  }

main = withSocketsDo $ do
         h <- connectTo host port
         hPrint h (show testMsg)
         hClose h
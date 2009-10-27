module Main()
    where

import Network
import System.IO
import Message
import Graphics.UI.Gtk

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
          
testSend = withSocketsDo $ do
  handle <- connectTo host port
  hPrint handle (show testMsg)
  hClose handle
  
testLogToConsole = do
  print ("Send transaction: " ++ (show testMsg))

main :: IO ()
main = do
  initGUI
  window <- windowNew
  button <- buttonNew
  set window [ containerBorderWidth := 10,
               containerChild := button ]
  set button [ buttonLabel := "Send transaction" ]
  onClicked button (testSend >> testLogToConsole)
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
module Main()
       where

import Network
import System.IO
import Message
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

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
  dialogXml <- xmlNew "transaction_dialog.glade"
  let dialogX = case dialogXml of
        (Just dialogX) -> dialogX
        Nothing        -> error "cant load the transaction_dialog.glade file."

  window <- xmlGetWidget dialogX castToWindow "transaction_dialog"
  button <- xmlGetWidget dialogX castToButton "conmmit_btn"
  onClicked button (testSend >> testLogToConsole)
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

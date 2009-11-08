module Main()
       where

import Network
import System.IO

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade ()

import Message
import TransactionDialog

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
  
  gui <- loadTransactionDialog "transaction_dialog.glade"
  
  setTransactionDialogData gui testTransaction
  
  onClicked (commit_btn gui) (testSend >> testLogToConsole)
  onDestroy (dialog_wnd gui) mainQuit
  onDestroy (cancel_btn gui) mainQuit
  widgetShowAll (dialog_wnd gui)
  mainGUI

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
                                  
                  
makeMessage :: Request -> Message
makeMessage request = Message { unp = 123456789
                              , body = show request
                              , digest = 0
                              }
                                                          
testSend message = withSocketsDo $ do
  handle <- connectTo host port
  hPrint handle (show message)
  hClose handle
                                                  
testLogToConsole message= do
  print ("Send transaction: " ++ (show message))
  
  
onCommitTransactionClicked :: TransactionDialog -> IO ()
onCommitTransactionClicked gui = do
  trans <- getTransactionDialogData gui
  let msg = makeMessage $ CommitTransaction trans
  (testSend msg) >> (testLogToConsole msg)
  

main :: IO ()
main = do
  initGUI
  
  gui <- loadTransactionDialog "transaction_dialog.glade"
  setTransactionDialogData gui testTransaction
  
  trans <- getTransactionDialogData gui
  let msg = makeMessage $ CommitTransaction trans
  
  onClicked (commit_btn gui) $ onCommitTransactionClicked gui
  onDestroy (dialog_wnd gui) mainQuit
  onClicke  (cancel_btn gui) mainQuit
  widgetShowAll (dialog_wnd gui)
  mainGUI

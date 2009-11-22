module Main()
       where

import Network
import System.IO

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade ()

import Message
import TransactionDialog
import MainWindow

host = "127.0.0.1"
port = PortNumber 6555


                                                                                       
testSend message = withSocketsDo $ do
	handle <- connectTo host port
	hPrint handle (show message)
	hClose handle
                                                  


makeMessage :: Integer -> Request -> Message
makeMessage userUnp request = Message { unp = userUnp
                              , body = show request
                              , digest = 0
                              }

    
onCommitTransactionClicked :: TransactionDialog -> IO ()
onCommitTransactionClicked gui = do
    trans <- getTransactionDialogData gui
    let userUnp = 123456789
    let msg = makeMessage userUnp (CommitTransaction  trans)
    (testSend msg) >> (testLogToConsole msg)
  
    
testLogToConsole :: (Show a) => a -> IO ()
testLogToConsole message= do
  print ("Send transaction: " ++ (show message))
  
  

bindActions :: ActionGroup -> IO ()
bindActions actions = do
    (Just payAction) <- actionGroupGetAction actions "NewPay_a"
    onActionActivate payAction (putStrLn "Transaction dialog launch!")--(onNewTransaction onCommitTransactionClicked)
    return ()

  

main :: IO ()
main = do
    initGUI
    gui <- loadMainWindow
    bindActions   (actions gui)
    onDestroy     (window  gui) mainQuit
    widgetShowAll (window  gui)
    mainGUI

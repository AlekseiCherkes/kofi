import Network
import System.IO

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade ()

-- Common imports
import Types
import Validation
import Message

-- Client imports
import TransactionDialog
import BalanceDialog
import MainWindow

host = "127.0.0.1"
port = PortNumber 6555



testSend message = withSocketsDo $ do
    handle <- connectTo host port
    hPrint handle (show message)
    hClose handle
                                                  

    
testLogToConsole :: (Show a) => a -> IO ()
testLogToConsole message= do
  print ("Send transaction: " ++ (show message))
  
  
  
makeMessage :: String -> Request -> Message
makeMessage unp request = Message { senderId = ClientId unp
                                  , body = show request
                                  , digest = "0"
                                  }

  
onCommitTransactionClicked :: CommitedTransaction -> IO ()
onCommitTransactionClicked trans = do
    let msg = makeMessage "123456789" (CommitTransaction trans)
    (testSend msg) >> (testLogToConsole msg)
  
  

bindActions :: ActionGroup -> IO ()
bindActions actions = do
    (Just payAction) <- actionGroupGetAction actions "NewPay_a"
    onActionActivate payAction (onNewTransaction onCommitTransactionClicked)
    
    (Just accAction) <- actionGroupGetAction actions "ViewBalance_a"
    onActionActivate accAction (showBalanceDialog $ str2unp "987654321123")
    return ()

  
main :: IO ()
main = do
    initGUI
    gui <- loadMainWindow
    bindActions   (actions gui)
    onDestroy     (window  gui) mainQuit
    widgetShowAll (window  gui)
    mainGUI

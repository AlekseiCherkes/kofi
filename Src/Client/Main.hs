
import System.IO

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade ()

-- Common imports
import Types ()
import Validation
import Message

-- Client imports
import ClientEntities
import ClientMessage
import TransactionDialog
import BalanceDialog
import MainWindow

  
onCommitTransactionClicked :: CommitedTransaction -> IO ()
onCommitTransactionClicked trans = do
    let msg = makeMessage "123456789" (CommitTransaction trans)
    (testSend msg) >> (testLogToConsole msg)
  
  

bindActions :: ActionGroup -> IO ()
bindActions actions = do
    (Just payAction) <- actionGroupGetAction actions "NewPay_a"
    onActionActivate payAction (onNewTransaction onCommitTransactionClicked)
    
    (Just accAction) <- actionGroupGetAction actions "ViewBalance_a"
    onActionActivate accAction (showBalanceDialog $ Session $ str2unp "987654321123")
    return ()

  
main :: IO ()
main = do
    initGUI
    gui <- loadMainWindow
    bindActions   (actions gui)
    onDestroy     (window  gui) mainQuit
    widgetShowAll (window  gui)
    mainGUI

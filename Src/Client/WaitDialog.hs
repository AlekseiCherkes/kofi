module WaitDialog where

import System.IO
import Control.Concurrent
import Control.Concurrent.MVar

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- Common imports
import Message

-- Client imports
import ClientEntities
import ClientMessage


data WaitDialog = WaitDialog { dialog_wnd :: Dialog,
                               close_btn  :: Button,
                               bar        :: ProgressBar
                             }


loadWaitDialog :: FilePath -> IO WaitDialog
loadWaitDialog gladePath = do
    Just glade  <- xmlNew gladePath
    dialog_wnd  <- xmlGetWidget glade castToDialog      "dialog_wnd"
    bar         <- xmlGetWidget glade castToProgressBar "bar" 
    close_btn   <- xmlGetWidget glade castToButton      "close_btn"

    return $ WaitDialog dialog_wnd close_btn bar



showWaitDialog :: (WindowClass t_parent) => t_parent -> Session -> Request -> IO (Maybe Response)
showWaitDialog parent session request = do
    mvar_block <- newEmptyMVar
    mvar_data  <- newEmptyMVar
    
    gui <- loadWaitDialog "Resources/wait_dialog.glade"
    windowSetTransientFor (dialog_wnd gui) parent
    widgetShowAll         (dialog_wnd gui)
    
    tid1 <- forkIO (doProgressBar (bar gui))
    tid2 <- forkIO (doSendRequest session request mvar_block mvar_data)
    
    let action = do
        mval <- tryTakeMVar mvar_block
        case mval of 
            Nothing -> do
                yield
                return True
            Just _ -> do
                dialogResponse (dialog_wnd gui) ResponseClose
                widgetDestroy  (dialog_wnd gui)
                killThread tid1
                killThread tid2
                return False
                
    timeoutAddFull action priorityDefaultIdle 50
    dialogRun (dialog_wnd gui)
    tryPutMVar  mvar_block ()
    tryTakeMVar mvar_data
     
    
doProgressBar :: ProgressBar -> IO ()
doProgressBar pb = do
    progressBarPulse pb
    threadDelay 10
    doProgressBar pb


doSendRequest :: Session -> Request -> MVar () -> MVar Response -> IO ()
doSendRequest session request mvar_block mvar_data = do
    resp <- sendRequest session request
    putMVar mvar_data  resp
    putMVar mvar_block ()
    
    
    
    
    









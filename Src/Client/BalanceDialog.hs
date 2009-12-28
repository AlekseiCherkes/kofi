module BalanceDialog where

-- standard imports
import System.IO
import Data.IORef

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- Common imports
import Types
import Validation

-- Client imports
import ClientEntities
import AccountChooser (showAccountChooser)




data BalanceDialog = BalanceDialog{ dialog_wnd    :: Dialog
                                  , bnk_lbl       :: Label
                                  , bic_lbl       :: Label
                                  , acc_lbl       :: Label
                                  , chooseAcc_btn :: Button
                                  , commit_btn    :: Button
                                  , cancel_btn    :: Button
                                  , selected_acc  :: IORef (Maybe AccountPK)
                                  }


loadBalanceDialog :: FilePath -> IO BalanceDialog
loadBalanceDialog gladePath = do
    Just glade <- xmlNew gladePath

    dialog_wnd <- xmlGetWidget glade castToDialog "dialog_wnd"
    [bnk_lbl      , bic_lbl   , acc_lbl   ] <- mapM (xmlGetWidget glade castToLabel ) ["bnk_lbl"      , "bic_lbl"   , "acc_lbl"   ]
    [chooseAcc_btn, commit_btn, cancel_btn] <- mapM (xmlGetWidget glade castToButton) ["chooseAcc_btn", "commit_btn", "cancel_btn"]

    accpk <- (newIORef Nothing)
    return $ BalanceDialog dialog_wnd bnk_lbl bic_lbl acc_lbl chooseAcc_btn commit_btn cancel_btn accpk
                

initBalanceDialog :: (IO (Maybe(AccountPK, Name))) -> (AccountPK -> IO()) -> BalanceDialog -> IO()
initBalanceDialog chooseAcc commitRequest gui = do
    mapM (\label -> labelSetText (label gui) "N/A") [bnk_lbl, bic_lbl, acc_lbl]
    
    onClicked (chooseAcc_btn gui) $ do
        putStrLn "Changing account..."
        chosenAcc <- chooseAcc
        putStrLn $ show chosenAcc
        case chosenAcc of
            Nothing   -> return ()
            Just pair -> updateAccountData gui pair
        
    
    let dialog = dialog_wnd gui
    onResponse dialog $ \responce -> do
        putStrLn "Dialog responce received."
        case responce of
            ResponseOk     -> do
                (putStrLn "Response Ok")
                maccpk <- readIORef $ selected_acc gui
                case (maccpk) of
                    Nothing    ->  (putStrLn "No account selected!")
                    Just accpk ->  do
                        commitRequest accpk
                        widgetDestroy dialog
            ResponseCancel -> do
                (putStrLn "Response Cancel")
                widgetDestroy dialog
            otherwise      -> return ()
     
    return ()
    
updateAccountData :: BalanceDialog -> (AccountPK, Name) -> IO ()
updateAccountData gui (accpk, name) = do
    writeIORef (selected_acc gui) (Just accpk)
    labelSetText (bnk_lbl gui)  name
    labelSetText (bic_lbl gui) (bankBic accpk)
    labelSetText (acc_lbl gui) (accId   accpk)    
        
    



showBalanceDialog :: Session -> IO()
showBalanceDialog session = do
    gui <- loadBalanceDialog "Resources/balanceRequest_dialog.glade"
    initBalanceDialog 
        (showAccountChooser session (dialog_wnd gui))
        (\_ ->  putStrLn "The request is to be commited...")
        gui
        
    widgetShowAll (dialog_wnd gui)
         

       







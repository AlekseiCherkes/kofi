module BalanceDialog where

-- standard imports
import System.IO

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- Common imports
import Types
import Validation







data BalanceDialog = BalanceDialog{ dialog_wnd    :: Dialog
                                  , bnk_lbl       :: Label
                                  , bic_lbl       :: Label
                                  , acc_lbl       :: Label
                                  , chooseAcc_btn :: Button
                                  , commit_btn    :: Button
                                  , cancel_btn    :: Button
                                  }


loadBalanceDialog :: FilePath -> IO BalanceDialog
loadBalanceDialog gladePath = do
    Just glade <- xmlNew gladePath

    dialog_wnd <- xmlGetWidget glade castToDialog "dialog_wnd"
    [bnk_lbl      , bic_lbl   , acc_lbl   ] <- mapM (xmlGetWidget glade castToLabel ) ["bnk_lbl"      , "bic_lbl"   , "acc_lbl"   ]
    [chooseAcc_btn, commit_btn, cancel_btn] <- mapM (xmlGetWidget glade castToButton) ["chooseAcc_btn", "commit_btn", "cancel_btn"]

    return $ BalanceDialog dialog_wnd bnk_lbl bic_lbl acc_lbl chooseAcc_btn commit_btn cancel_btn
                

initBalanceDialog :: (IO (AccountPK, Name)) -> (AccountPK -> IO()) -> BalanceDialog -> IO()
initBalanceDialog chooseAcc commitRequest gui = do
    mapM (\label -> labelSetText (label gui) "N/A") [bnk_lbl, bic_lbl, acc_lbl]
    
    onClicked (chooseAcc_btn gui) $ do
        putStrLn "Changing account..."
        (acc, bnk) <- chooseAcc
        labelSetText (bnk_lbl gui)  bnk
        labelSetText (bic_lbl gui) (bankBic acc)
        labelSetText (acc_lbl gui) (accId   acc)
    
    let dialog = dialog_wnd gui
    onResponse dialog $ \responce -> do
        putStrLn "Dialog responce received."
        case responce of
            ResponseOk     -> do
                (putStrLn "Response Ok")
                accpk <- getAccountData gui
                commitRequest accpk
            ResponseCancel -> do
                (putStrLn "Response Cancel")
            otherwise      -> return ()
        widgetDestroy dialog
     
    return ()
        
    

getAccountData :: BalanceDialog -> IO AccountPK
getAccountData gui = do
    acc <- labelGetText (acc_lbl gui)
    bic <- labelGetText (bic_lbl gui)
    return $ AccountPK  (str2acc acc) (str2bic bic)


showBalanceDialog :: UNP -> IO()
showBalanceDialog _ = do
    gui <- loadBalanceDialog "Resources/balanceRequest_dialog.glade"
    initBalanceDialog 
        (do
            putStrLn "Account chooser is to be shown..."
            return (AccountPK "123" "1111222233334", "Technodank"))
        (\_ ->  putStrLn "The request is to be commited...")
        gui
        
    widgetShowAll (dialog_wnd gui)
         
        







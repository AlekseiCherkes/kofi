module BalanceDialog where

-- standard imports
import System.IO
import Data.IORef

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- Common imports
import Types
import Message

-- Client imports
import GtkCommon
import ClientEntities
import AccountChooser (showAccountChooser)
import WaitDialog     (showWaitDialog)




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
    onClicked (chooseAcc_btn gui) $ do
        chosenAcc <- chooseAcc
        case chosenAcc of
            Nothing         -> return () --writeIORef (selected_acc gui) Nothing
            Just (accpk, _) -> do 
                writeIORef (selected_acc gui) (Just accpk) 
                renderAccountInfo chosenAcc (bnk_lbl gui) (bic_lbl gui) (acc_lbl gui)
        validateBalanceDialog gui


    let dialog = dialog_wnd gui
    onResponse dialog $ \responce -> do
        case responce of
            ResponseOk     -> do
                maccpk <- readIORef $ selected_acc gui
                case (maccpk) of
                    Nothing    ->  (putStrLn "No account selected!")
                    Just accpk ->  do
                        commitRequest accpk
                        widgetDestroy dialog
            ResponseCancel -> widgetDestroy dialog
            otherwise      -> return ()

    return ()

validateBalanceDialog :: BalanceDialog -> IO ()
validateBalanceDialog gui = (setButtonSensitive $ commit_btn gui) =<< (isRefSet $ selected_acc gui)


commitRequest :: BalanceDialog -> Session -> AccountPK -> IO ()
commitRequest gui session accpk = do
    mservResp <- showWaitDialog (dialog_wnd gui) session (GetBalance accpk)
    dialog <- case mservResp of
        Nothing               -> messageDialogNew Nothing [DialogModal] MessageInfo  ButtonsOk ("Запрос был отменен.")
        Just (Balance amount) -> messageDialogNew Nothing [DialogModal] MessageInfo  ButtonsOk ("Баланс вашего счета: " ++ show amount ++ ".")
        Just (Error   msg   ) -> messageDialogNew Nothing [DialogModal] MessageError ButtonsClose ("Ошибка: " ++ msg ++ ".")
        otherwise             -> do
            msg_dialog <- messageDialogNew Nothing [DialogModal] MessageError ButtonsClose ("Сервер ответил неверно: ")
            messageDialogSetSecondaryText msg_dialog (show mservResp)
            return msg_dialog
    
    windowSetTransientFor dialog (dialog_wnd gui)
    dialogRun dialog
    widgetDestroy dialog


showBalanceDialog :: Session -> IO()
showBalanceDialog session = do
    gui <- loadBalanceDialog "Resources/balanceRequest_dialog.glade"
    initBalanceDialog
        (showAccountChooser (dialog_wnd gui) session (profileUnp $ sessionProfile session) )
        (commitRequest gui session)
        gui

    validateBalanceDialog gui
    widgetShowAll (dialog_wnd gui)
            








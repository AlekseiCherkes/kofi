module TransactionDialog
       where

import System.IO
import Data.IORef
import Data.List (findIndex)
import Control.Monad (liftM2)

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade


-- Common imports
import Types
import Validation
import ClientEntities
import DataModel


-- Client imports
import Message
import GtkCommon
import DataModel
import AccountChooser  (showAccountChooser)
import WaitDialog      (showWaitDialog)
import TemplateChooser (showTemplateChooser)
import TemplateSaver   (showTemplateSaver)

import Printers

import Debug.Trace

data TransactionDialog = TransactionDialog{ dialog_wnd         :: Dialog
                                           ,commit_btn         :: Button
                                           ,cancel_btn         :: Button
                                           ,save_btn           :: Button
                                           ,load_btn           :: Button
                                           ,changePayerAcc_btn :: Button
                                           ,changePayeeAcc_btn :: Button
                                           ,urgent_btn         :: RadioButton
                                           ,notUrgent_btn      :: RadioButton
                                           ,amount_entry       :: Entry
                                           ,payerBank_lbl      :: Label
                                           ,payerBankBic_lbl   :: Label
                                           ,payerAcc_lbl       :: Label
                                           ,payeeName_lbl      :: Label
                                           ,payeeBank_lbl      :: Label
                                           ,payeeBankBic_lbl   :: Label
                                           ,payeeAcc_lbl       :: Label
                                           ,payee_cmb          :: ComboBox --Entry
                                           ,reason_txt         :: TextView
                                           ,payer_acc          :: IORef (Maybe AccountPK)
                                           ,payee_acc          :: IORef (Maybe AccountPK)
                                           ,payee_unp          :: IORef (Maybe UNP)
                                          }




loadTransactionDialog :: FilePath -> IO TransactionDialog
loadTransactionDialog gladePath = do
    Just glade <- xmlNew gladePath

    dialog_wnd   <- xmlGetWidget glade castToDialog        "dialog_wnd"
    reason_txt   <- xmlGetWidget glade castToTextView      "reason_txt"
    amount_entry <- xmlGetWidget glade castToEntry         "amount_entry"
    payee_cmb    <- xmlGetWidget glade castToComboBox      "payee_cmb"

    [ commit_btn         ,  cancel_btn         ,
      save_btn           ,  load_btn           ,
      changePayerAcc_btn ,  changePayeeAcc_btn ] <- mapM (xmlGetWidget glade castToButton) [
     "commit_btn"        , "cancel_btn"        ,
     "save_btn"          , "load_btn"          ,
     "changePayerAcc_btn", "changePayeeAcc_btn"]

    [ urgent_btn ,  notUrgent_btn ] <- mapM (xmlGetWidget glade castToRadioButton) [
     "urgent_btn", "notUrgent_btn"]

    [ payerBank_lbl ,  payerBankBic_lbl ,  payerAcc_lbl ,
      payeeBank_lbl ,  payeeBankBic_lbl ,  payeeAcc_lbl ,
      payeeName_lbl                                     ] <- mapM (xmlGetWidget glade castToLabel) [
     "payerBank_lbl", "payerBankBic_lbl", "payerAcc_lbl",
     "payeeBank_lbl", "payeeBankBic_lbl", "payeeAcc_lbl",
     "payeeName_lbl"                                    ]

    radioButtonSetGroup urgent_btn notUrgent_btn

    payer_acc <- (newIORef Nothing)
    payee_acc <- (newIORef Nothing)
    payee_unp <- (newIORef Nothing)


    return $ TransactionDialog
                dialog_wnd
                commit_btn
                cancel_btn
                save_btn
                load_btn
                changePayerAcc_btn
                changePayeeAcc_btn
                urgent_btn
                notUrgent_btn
                amount_entry
                payerBank_lbl
                payerBankBic_lbl
                payerAcc_lbl
                payeeName_lbl
                payeeBank_lbl
                payeeBankBic_lbl
                payeeAcc_lbl
                payee_cmb
                reason_txt
                payer_acc
                payee_acc
                payee_unp



initTransactionDialog :: TransactionDialog -> (Session -> UNP -> IO (Maybe (AccountPK, Name))) -> Session -> IO ()
initTransactionDialog gui chooseAcc session = do
    let path = sessionPath session
    
    onClicked (changePayerAcc_btn gui) $ do
        chosenAcc <- chooseAcc session  (profileUnp $ sessionProfile session)
        case chosenAcc of
            Nothing         -> return ()
            Just (accpk, _) -> do
                writeIORef (payer_acc gui) (Just accpk)
                renderAccountInfo chosenAcc (payerBank_lbl gui) (payerBankBic_lbl gui) (payerAcc_lbl gui)
        validateTransactionDialog gui
        return ()

    onClicked (changePayeeAcc_btn gui) $ do
        munp <- readIORef (payee_unp gui)
        case munp of
            Nothing  -> return ()
            Just unp -> do
                chosenAcc <- chooseAcc session unp
                case chosenAcc of
                    Nothing         -> return ()
                    Just (accpk, _) -> do
                        writeIORef (payee_acc gui) (Just accpk)
                        renderAccountInfo chosenAcc (payeeBank_lbl gui) (payeeBankBic_lbl gui) (payeeAcc_lbl gui)
                validateTransactionDialog gui
                return()

    model <- listStoreNew =<< fetchAllCompanies path
    let combo = payee_cmb gui
    initPayeesCombobox combo model $ do
        writeIORef (payee_acc gui) Nothing
        i <- comboBoxGetActive combo
        if i == -1
            then do
                writeIORef (payee_unp gui) Nothing
                labelSetText (payeeName_lbl gui) "N/A"
            else do
                cmp <- listStoreGetValue model i 
                writeIORef   (payee_unp     gui) (Just $ cmpUnp cmp)
                labelSetText (payeeName_lbl gui) (cmpName cmp)
        validateTransactionDialog gui
        return ()


    onClicked (commit_btn gui) $ do
        isValid <- validateTransactionDialog gui
        if isValid 
            then do
                commitTransaction gui session
                dialogResponse (dialog_wnd  gui) ResponseOk
            else showWarningMessage gui

    onClicked (cancel_btn gui) (dialogResponse (dialog_wnd  gui) ResponseCancel)

    onClicked (save_btn gui) $ do
        isValid <- validateTransactionDialog gui
        if isValid 
            then do
                mname <- showTemplateSaver (dialog_wnd  gui)
                case mname of
                    Nothing   -> return ()
                    Just name -> do
                        tmpl <- getTemplateData gui name
                        insertTransactionTemplate path tmpl
            else showWarningMessage gui
            

    onClicked (load_btn gui) $ do
        mtmpl <- showTemplateChooser (dialog_wnd gui) session
        case mtmpl of
            Nothing   -> return ()
            Just tmpl -> setTransactionDialodData gui model session tmpl

    return ()
    
showWarningMessage :: TransactionDialog -> IO ()
showWarningMessage gui = do
    msg_dialog <- messageDialogNew Nothing [DialogModal] MessageWarning ButtonsClose "Платежное поручение неверно заполнено."
    messageDialogSetSecondaryText msg_dialog "Возможно, неверно указана сумма или пропущено назначение платежа."
    windowSetTransientFor msg_dialog (dialog_wnd gui)
    dialogRun msg_dialog
    widgetDestroy msg_dialog
    


initPayeesCombobox :: ComboBox -> ListStore Company -> IO () -> IO ()
initPayeesCombobox combo model cangeHandler = do
    comboBoxSetModel combo $ Just model
    renderer <- cellRendererTextNew
    cellLayoutPackStart combo renderer True
    cellLayoutSetAttributes combo renderer model $ \row -> [ cellText := (unp2str . cmpUnp) row ]

    on combo changed cangeHandler
    return ()

validateTransactionDialog :: TransactionDialog -> IO Bool
validateTransactionDialog gui = do
    let isSet = \getter -> isRefSet $ getter gui
    let andM  = liftM2 (&&)
    cond <-    isSet payee_unp 
        `andM` isSet payer_acc 
        `andM` isSet payee_acc

    setButtonSensitive (changePayeeAcc_btn gui) =<< isSet payee_unp
    setButtonSensitive (commit_btn         gui) cond
    setButtonSensitive (save_btn           gui) cond
    
    (return cond)
        `andM` (return . not . null    =<< (getMultilineText .  reason_txt  ) gui)
        `andM` (return . isValidAmount =<< (entryGetText     .  amount_entry) gui)


setTransactionDialodData :: TransactionDialog -> ListStore Company -> Session -> TransactionTemplate -> IO ()
setTransactionDialodData gui model session tmpl = do
    let path       = sessionPath session
    let payerAccPk = transactionTemplatePayerAccountPK tmpl
    let payeeAccPk = transactionTemplateBnfcAccountPK  tmpl
    
    [payerBnk, payeeBnk] <- mapM (\apk -> (return . bnkName) =<< (findBankByBic . bankBic) apk) [payerAccPk, payeeAccPk]
    unp   <- return . accCompany =<< (findAccount path (str2bic "151501267") (accId payeeAccPk))
    cmp   <- findCompanyByUnp path unp 

    unps  <- return . (map cmpUnp) =<<  listStoreToList model
    idx   <- case (findIndex ((==) unp) unps) of
      Just idx -> return idx
      Nothing  -> return (-1)
      
    comboBoxSetActive     (payee_cmb     gui) idx
    labelSetText          (payeeName_lbl gui) (cmpName cmp)
    writeIORef            (payer_acc     gui) (Just payerAccPk)
    writeIORef            (payee_acc     gui) (Just payeeAccPk)
    
    renderAccountInfo (Just (payerAccPk, payerBnk)) (payerBank_lbl gui) (payerBankBic_lbl gui) (payerAcc_lbl gui)
    renderAccountInfo (Just (payeeAccPk, payeeBnk)) (payeeBank_lbl gui) (payeeBankBic_lbl gui) (payeeAcc_lbl gui)
    
    setMultilineText      (reason_txt    gui) (transactionTemplateReason        tmpl)
    entrySetText          (amount_entry  gui) (show $ transactionTemplateAmount tmpl)
    toggleButtonSetActive (urgent_btn    gui) (transactionTemplateIsUrgent      tmpl)
    
    validateTransactionDialog gui
    return ()
    
    

getTransactionDialogData :: TransactionDialog -> IO CommitedTransaction
getTransactionDialogData gui = do
    reason         <- (getMultilineText . reason_txt) gui
    Just creditAcc <- (readIORef        . payer_acc ) gui
    Just debitAcc  <- (readIORef        . payee_acc ) gui
    amount         <- (return . (read::String -> Double)) =<< (entryGetText . amount_entry) gui
    isUrgent       <- (toggleButtonGetActive . urgent_btn) gui
    
    return $ CommitedTransaction reason creditAcc debitAcc amount (
                case isUrgent of
                    True  -> Urgent
                    False -> Normal)  
                    
getTemplateData :: TransactionDialog -> String -> IO TransactionTemplate         
getTemplateData gui name = do
    reason         <- (getMultilineText . reason_txt) gui
    Just creditAcc <- (readIORef        . payer_acc ) gui
    Just debitAcc  <- (readIORef        . payee_acc ) gui
    amount         <- (return . (read::String -> Double)) =<< (entryGetText . amount_entry) gui
    isUrgent       <- (toggleButtonGetActive . urgent_btn) gui
    
    return $ TransactionTemplate
                0         name
                creditAcc debitAcc
                amount    reason 
                isUrgent  
    


commitTransaction :: TransactionDialog -> Session -> IO ()
commitTransaction gui session = do
    trans <- getTransactionDialogData gui
    
    printTransaction (sessionPath session) trans >>= putTraceMsg
    
    mservResp <- showWaitDialog (dialog_wnd gui) session (CommitTransaction trans)
    dialog <- case mservResp of
        Nothing               -> messageDialogNew Nothing [DialogModal] MessageInfo  ButtonsOk ("Запрос был отменен.")
        Just (Silence)        -> messageDialogNew Nothing [DialogModal] MessageInfo  ButtonsOk ("Плетежное поручение отправлено.")
        Just (Error   msg   ) -> messageDialogNew Nothing [DialogModal] MessageError ButtonsClose ("Ошибка: " ++ msg ++ ".")
        otherwise             -> do
            msg_dialog <- messageDialogNew Nothing [DialogModal] MessageError ButtonsClose ("Сервер ответил неверно: ")
            messageDialogSetSecondaryText msg_dialog (show mservResp)
            return msg_dialog
    
    windowSetTransientFor dialog (dialog_wnd gui)
    dialogRun dialog
    widgetDestroy dialog


showTransactionDialog :: (WindowClass twin) => twin -> Session -> IO ()
showTransactionDialog parent session = do
    gui <- loadTransactionDialog "Resources/transaction_dialog.glade"
    initTransactionDialog gui (showAccountChooser $ dialog_wnd gui) session
    validateTransactionDialog gui
    onResponse (dialog_wnd gui) (\_ -> widgetDestroy $ dialog_wnd gui)
    windowSetTransientFor (dialog_wnd gui) parent
    windowSetDestroyWithParent (dialog_wnd gui) True
    widgetShowAll (dialog_wnd gui)




module TransactionDialog
       where

import System.IO
import Data.IORef

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade


-- Common imports
import Types
import ClientEntities
import DataModel ()


-- Client imports
import Message
import GtkCommon




data TransactionDialog = TransactionDialog{ dialog_wnd         :: Dialog
                                           ,commit_btn         :: Button
                                           ,cancel_btn         :: Button
                                           ,saveTemplate_btn   :: Button
                                           ,loadTemplate_btn   :: Button
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
                                           ,payee_cmb          :: ComboBoxEntry
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
    payee_cmb    <- xmlGetWidget glade castToComboBoxEntry "payee_cmb"
    
    [ commit_btn         ,  cancel_btn         , 
      saveTemplate_btn   ,  loadTemplate_btn   , 
      changePayerAcc_btn ,  changePayeeAcc_btn ] <- mapM (xmlGetWidget glade castToButton) [
     "commit_btn"        , "cancel_btn"        , 
     "saveTemplate_btn"  , "loadTemplate_btn"  , 
     "changePayerAcc_btn", "changePayeeAcc_btn"]
     
    [ urgent_btn ,  notUrgent_btn ] <- mapM (xmlGetWidget glade castToRadioButton) [
     "urgent_btn", "notUrgent_btn"]
     
    [ payerBank_lbl ,  payerBankBic_lbl ,  payerAcc_lbl ,
      payeeBank_lbl ,  payeeBankBic_lbl ,  payeeAcc_lbl ,
      payeeName_lbl                                     ] <- mapM (xmlGetWidget glade castToLabel) [
     "payerBank_lbl", "payerBankBic_lbl", "payerAcc_lbl",
     "payeeBank_lbl", "payeeBankBic_lbl", "payeeAcc_lbl",
     "payeeName_lbl"                                    ] 
     
    payer_acc <- (newIORef Nothing)
    payee_acc <- (newIORef Nothing)
    payee_unp <- (newIORef Nothing)
    
     
    return $ TransactionDialog
                dialog_wnd
                commit_btn
                cancel_btn
                saveTemplate_btn
                loadTemplate_btn
                changePayerAcc_btn
                changePayeeAcc_btn
                urgent_btn
                notUrgent_btn
                amount_entry
                payeeName_lbl
                payerBank_lbl
                payerBankBic_lbl
                payerAcc_lbl
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
    onClicked (changePayerAcc_btn gui) $ (do
        chosenAcc <- chooseAcc session  (profileUnp $ sessionProfile session) 
        case chosenAcc of
            Nothing         -> writeIORef (payer_acc gui) Nothing
            Just (accpk, _) -> writeIORef (payer_acc gui) (Just accpk)
        renderAccountInfo chosenAcc (payerBank_lbl gui) (payerBankBic_lbl gui) (payerAcc_lbl gui));
                
    onClicked (changePayeeAcc_btn gui) $ (do
        munp <- readIORef (payee_unp gui)
        case munp of
            Nothing  -> return ()
            Just unp -> do
                chosenAcc <- chooseAcc session unp
                case chosenAcc of
                    Nothing         -> writeIORef (payee_acc gui) Nothing
                    Just (accpk, _) -> writeIORef (payee_acc gui) (Just accpk)
                renderAccountInfo chosenAcc (payeeBank_lbl gui) (payeeBankBic_lbl gui) (payeeAcc_lbl gui));
     return ()           
   
        
initPayeesCombobox :: ComboBox -> ListStore UNP -> IO () -> IO ()
initPayeesCombobox combo model cangeHandler = do
    comboBoxSetModel combo $ Just model
    renderer <- cellRendererTextNew
    cellLayoutPackStart combo renderer True
    cellLayoutSetAttributes combo renderer model $ \row -> [ cellText := unp2str row ]
    
    on combo changed cangeHandler
--    on combo changed $ do
--        writeIORef (payee_acc gui) Nothing
--        i <- comboBoxGetActive combo
--        if i == -1 
--            then do
--                writeIORef (payee_unp gui) Nothing
--            else do
--                unp <- listStoreGetValue model
--                writeIORef (payee_unp gui) (Just unp)
                
    return ()
                

getTransactionDialogData :: TransactionDialog -> IO CommitedTransaction
getTransactionDialogData gui = return $   
    CommitedTransaction { reason = "test this client server communication"
                        , creditAccount = AccountPK (str2acc "123456789") (str2bic "000000001")
                        , debitAccount  = AccountPK (str2acc "987654321") (str2bic "000000001")
                        , amount = 100.0
                        , priority = Normal
                        }
                        
                        
--onCommitTransactionClicked :: CommitedTransaction -> IO ()
--onCommitTransactionClicked trans = do
--    let msg = makeMessage "123456789" (CommitTransaction trans)
--    (testSend msg) >> (testLogToConsole msg)


showTransactionDialog :: Session -> IO ()
showTransactionDialog session = do
    gui <- loadTransactionDialog "Resources/transaction_dialog.glade"
    widgetShowAll (dialog_wnd gui)




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
                                           ,payeeBank_lbl      :: Label
                                           ,payeeBankBic_lbl   :: Label
                                           ,payeeAcc_lbl       :: Label
                                           ,payee_cmb          :: ComboBoxEntry
                                           ,reason_txt         :: TextView
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
      payeeBank_lbl ,  payeeBankBic_lbl ,  payeeAcc_lbl ] <- mapM (xmlGetWidget glade castToLabel) [
     "payerBank_lbl", "payerBankBic_lbl", "payerAcc_lbl",
     "payeeBank_lbl", "payeeBankBic_lbl", "payeeAcc_lbl"] 
     
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
                payerBank_lbl
                payerBankBic_lbl
                payerAcc_lbl
                payeeBank_lbl
                payeeBankBic_lbl
                payeeAcc_lbl
                payee_cmb
                reason_txt





getTransactionDialogData :: TransactionDialog -> IO CommitedTransaction
getTransactionDialogData gui = return $   
    CommitedTransaction { reason = "test this client server communication"
                        , creditAccount = AccountPK "123456789" "000000001"
                        , debitAccount  = AccountPK "987654321" "000000001"
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

 






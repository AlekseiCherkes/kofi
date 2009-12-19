module TransactionDialog
       where


import System.IO
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import Types
import ClientEntities
import DataModel ()
import Message
import GtkCommon




data TransactionDialog = TransactionDialog{ dialog_wnd         :: Dialog
                                           ,commit_btn         :: Button
                                           ,cancel_btn         :: Button
                                           ,urgent_btn         :: RadioButton
                                           ,notUrgent_btn      :: RadioButton
                                           ,currency_cmb       :: ComboBoxEntry
                                           ,amount_entry       :: Entry
                                           ,payer_cmb          :: ComboBoxEntry
                                           ,payerAcc_cmb       :: ComboBoxEntry
                                           ,payerBank_cmb      :: ComboBoxEntry
                                           ,payerBankBic_entry :: Entry
                                           ,payeeBank_cmb      :: ComboBoxEntry
                                           ,payeeBankBic_entry :: Entry
                                           ,payee_cmb          :: ComboBoxEntry
                                           ,payeeAcc_cmb       :: ComboBoxEntry
                                           ,reason_txt         :: TextView
                                           ,payer_unp          :: Entry
                                           ,payee_unp          :: Entry
                                          }




loadTransactionDialog :: FilePath -> IO TransactionDialog
loadTransactionDialog gladePath = do
    Just glade <- xmlNew gladePath

    dialog_wnd         <- xmlGetWidget glade castToDialog "dialog"
    commit_btn         <- xmlGetWidget glade castToButton "commit_btn"
    cancel_btn         <- xmlGetWidget glade castToButton "cancel_btn"
    urgent_btn         <- xmlGetWidget glade castToRadioButton "urgent_btn"
    notUrgent_btn      <- xmlGetWidget glade castToRadioButton "notUrgent_btn"
    currency_cmb       <- xmlGetWidget glade castToComboBoxEntry "currency_cmb"
    payer_cmb          <- xmlGetWidget glade castToComboBoxEntry "payer_cmb"
    payee_cmb          <- xmlGetWidget glade castToComboBoxEntry "payee_cmb"
    payerAcc_cmb       <- xmlGetWidget glade castToComboBoxEntry "payerAcc_cmb"
    payeeAcc_cmb       <- xmlGetWidget glade castToComboBoxEntry "payeeAcc_cmb"
    payerBank_cmb      <- xmlGetWidget glade castToComboBoxEntry "payerBank_cmb"
    payeeBank_cmb      <- xmlGetWidget glade castToComboBoxEntry "payeeBank_cmb"
    amount_entry       <- xmlGetWidget glade castToEntry "amount_entry"
    payerBankBic_entry <- xmlGetWidget glade castToEntry "payerBankBic_entry"
    payeeBankBic_entry <- xmlGetWidget glade castToEntry "payeeBankBic_entry"
    payer_unp          <- xmlGetWidget glade castToEntry "payer_unp"
    payee_unp          <- xmlGetWidget glade castToEntry "payee_unp"
    reason_txt         <- xmlGetWidget glade castToTextView "reason_txt"

    return $ TransactionDialog
                dialog_wnd
                commit_btn
                cancel_btn
                urgent_btn
                notUrgent_btn
                currency_cmb
                amount_entry
                payer_cmb
                payerAcc_cmb
                payerBank_cmb
                payerBankBic_entry
                payeeBank_cmb
                payeeBankBic_entry
                payee_cmb
                payeeAcc_cmb
                reason_txt
                payer_unp
                payee_unp



setTransactionDialogData :: TransactionDialog -> CommitedTransaction -> IO ()
setTransactionDialogData gui trans = do
    let banks = []

    let creditAcc = creditAccount trans
    let debitAcc  = debitAccount  trans

    setComboEntryItems (payerAcc_cmb  gui) [accId $ creditAcc]
    setComboEntryItems (payeeAcc_cmb  gui) [accId $ debitAcc ]
    setComboEntryItems (payerBank_cmb gui) $ map (bnkName) banks
    setMultilineText   (reason_txt    gui) (reason trans)

    entrySetText (amount_entry gui) $ show (amount trans)
    radioButtonSetGroup   (urgent_btn gui) (notUrgent_btn gui)
    toggleButtonSetActive (urgent_btn gui) (isUrgent (priority trans))



getTransactionDialogData :: TransactionDialog -> IO CommitedTransaction
getTransactionDialogData gui = do   
    Just payerAccNum <- comboBoxGetActiveText (payerAcc_cmb gui)
    Just payeeAccNum <- comboBoxGetActiveText (payeeAcc_cmb gui)
    reason           <- getMultilineText      (reason_txt   gui)
    amountTxt        <- entryGetText          (amount_entry gui)
    isUrgent         <- toggleButtonGetActive (urgent_btn   gui)

    let priority = case isUrgent of
            True -> Urgent
            False-> Normal    

    let amount   = (read amountTxt )::Double
    let payerAccPk = AccountPK payerAccNum "001"
    let payeeAccPk = AccountPK payeeAccNum "001"
    
    return $ CommitedTransaction reason payerAccPk payeeAccPk amount priority




onNewTransaction :: (CommitedTransaction -> IO ()) -> IO ()
onNewTransaction commit = do
    gui <- loadTransactionDialog "Resources/transaction_dialog.glade"
    setTransactionDialogData gui testTransaction
    onResponse    (dialog_wnd gui) (onTransactionResponse commit gui)
    widgetShowAll (dialog_wnd gui)


onTransactionResponse :: (CommitedTransaction -> IO ()) -> TransactionDialog -> ResponseId -> IO ()
onTransactionResponse commit gui responce = do
    let dialog = dialog_wnd gui
    case responce of
        ResponseOk     -> do
            (putStrLn "Response Ok")
            trans <- getTransactionDialogData gui
            commit trans
            widgetDestroy dialog
        ResponseCancel -> do
            (putStrLn "Response Cancel")
            widgetDestroy dialog
        otherwise      -> return ()



testTransaction = CommitedTransaction { reason = "test this client server communication"
                                      , creditAccount = AccountPK "123456789" "000000001"
                                      , debitAccount  = AccountPK "987654321" "000000001"
                                      , amount = 100.0
                                      , priority = Normal
                                      }






module TransactionDialog
       where


import System.IO
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import Message



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
                
 
setComboEntryItems :: ComboBoxEntry -> [String] -> IO ()
setComboEntryItems combo items = do
    comboBoxEntrySetModelText combo 
    mapM_ (comboBoxAppendText combo) items
    comboBoxSetActive combo 0


setMultilineText :: TextView -> String -> IO ()
setMultilineText textView text = do
    buffer <- textViewGetBuffer textView
    textBufferSetText buffer text
    

getMultilineText :: TextView -> IO String
getMultilineText textView = do
	buffer <- textViewGetBuffer textView
	start  <- textBufferGetStartIter buffer
	end    <- textBufferGetEndIter   buffer
	txt    <- textBufferGetText      buffer start end True
	return txt
    
 
setTransactionDialogData :: TransactionDialog -> CommitedTransaction -> IO ()
setTransactionDialogData gui trans = do
    setComboEntryItems (payerAcc_cmb gui) [show (creditAccountId trans)]
    setComboEntryItems (payeeAcc_cmb gui) [show (debitAccountId  trans)]
    
    setMultilineText (reason_txt gui) (reason trans)
    
    entrySetText (amount_entry gui) $ show (amount trans)
    radioButtonSetGroup   (urgent_btn gui) (notUrgent_btn gui)
    toggleButtonSetActive (urgent_btn gui) (isUrgent (priority trans))
    
    
getTransactionDialogData :: TransactionDialog -> IO CommitedTransaction
getTransactionDialogData gui = do
	Just payerAcc <- comboBoxGetActiveText (payerAcc_cmb gui)
	Just payeeAcc <- comboBoxGetActiveText (payeeAcc_cmb gui) 
	reason        <- getMultilineText      (reason_txt   gui)
	ammountTxt    <- entryGetText          (amount_entry gui)
	isUrgent      <- toggleButtonGetActive (urgent_btn   gui)
	
	let payerAccNum = (read payerAcc)  ::Integer
	let payeeAccNum = (read payeeAcc)  ::Integer
	let ammount     = (read ammountTxt)::Double
	
	let priority = case isUrgent of
		True -> Urgent
		False-> Normal
		
	return (CommitedTransaction reason payerAccNum payeeAccNum ammount priority)
    


       
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
                                      , creditAccountId = 123456789
                                      , debitAccountId = 987654321
                                      , amount = 100.0
                                      , priority = Urgent
                                      }

    
  



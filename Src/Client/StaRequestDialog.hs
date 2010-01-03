
module StaRequestDialog where

-- standard imports
import System.IO
import Data.IORef

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- Common imports
import Types


-- Client imports
import ClientEntities
import AccountChooser (showAccountChooser)


data StaRequestDialog = StaRequestDialog { dialog_wnd    :: Dialog
                                         , changeAcc_btn :: Button
                                         , commit_btn    :: Button
                                         , cancel_btn    :: Button
                                         , bank_lbl      :: Label
                                         , bic_lbl       :: Label
                                         , acc_lbl       :: Label
                                         , from_clnd     :: Calendar
                                         , till_clnd     :: Calendar
                                         }
                                         

loadStaRequestDialog :: FilePath -> IO StaRequestDialog
loadStaRequestDialog gladePath = do
    Just glade <- xmlNew gladePath
  
    dialog_wnd   <- xmlGetWidget glade castToDialog        "dialog_wnd"
    
    [ changeAcc_btn ,  commit_btn ,  cancel_btn ] <- mapM (xmlGetWidget glade castToButton) [
     "changeAcc_btn", "commit_btn", "cancel_btn"]
     
    [ bank_lbl ,  bic_lbl ,  acc_lbl ] <- mapM (xmlGetWidget glade castToLabel) [
     "bank_lbl", "bic_lbl", "acc_lbl"] 
     
    [ from_clnd ,  till_clnd ]  <- mapM (xmlGetWidget glade castToCalendar) [
     "from_clnd", "till_clnd"] 
     
    return $ StaRequestDialog 
                dialog_wnd
                changeAcc_btn
                commit_btn
                cancel_btn
                bank_lbl
                bic_lbl
                acc_lbl
                from_clnd
                till_clnd
    


showStaRequestDialog :: Session -> IO ()
showStaRequestDialog session = do
    gui <- loadStaRequestDialog "Resources/staRequest_dialog.glade"
    widgetShowAll (dialog_wnd gui)

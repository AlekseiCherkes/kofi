
module ProfileChooser where

import System.Time

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- Common imports
import Validation

-- Client imports
import ClientEntities


data ProfileChooserDialog = ProfileChooserDialog { dialog_wnd  :: Dialog
                                                 , ok_btn      :: Button
                                                 , close_btn   :: Button
                                                 , name_lbl    :: Label
                                                 , unp_lbl     :: Label
                                                 , date_lbl    :: Label
                                                 , profiles_tv :: TreeView
                                                 }
                                                 
loadProfileChooser :: FilePath -> IO ProfileChooserDialog
loadProfileChooser gladePath = do
    Just glade <- xmlNew gladePath
  
    dialog_wnd  <- xmlGetWidget glade castToDialog   "dialog_wnd"
    profiles_tv <- xmlGetWidget glade castToTreeView "profiles_tv"
    
    [ ok_btn ,  close_btn ] <- mapM (xmlGetWidget glade castToButton) [
     "ok_btn", "close_btn"]
     
    [ name_lbl ,  unp_lbl ,  date_lbl ] <- mapM (xmlGetWidget glade castToLabel) [
     "name_lbl", "unp_lbl", "date_lbl"]
          
    return $ ProfileChooserDialog dialog_wnd ok_btn close_btn name_lbl unp_lbl date_lbl profiles_tv


showProfileChooser :: IO (Maybe Session)
showProfileChooser = do
    
    gui <- loadProfileChooser "Resources/profileChooser_dialog.glade"
    widgetShowAll (dialog_wnd gui) 
    clock <- getClockTime
    time  <- toCalendarTime clock 
    let session = Session  (Profile (str2unp "987654321123") "Some company." time) "FilePath"
    
    return $ Just session

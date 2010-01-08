
module TemplateChooser where

import Data.IORef
import Data.List ( isPrefixOf, isSuffixOf)

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- Common imports


-- Client imports
import ClientEntities
import DataModel
import GtkCommon



data TemplateChooserDialog = TemplateChooserDialog { dialog_wnd       :: Dialog
                                                   , ok_btn           :: Button
                                                   , cancel_btn       :: Button
                                                   , templates_tv     :: TreeView
                                                   , selected_template:: IORef (Maybe TransactionTemplate ) 
                                                   }
                                                 
loadTemplateChooser :: FilePath -> IO TemplateChooserDialog
loadTemplateChooser gladePath = do
    Just glade <- xmlNew gladePath
  
    dialog_wnd   <- xmlGetWidget glade castToDialog   "dialog_wnd"
    templates_tv <- xmlGetWidget glade castToTreeView "templates_tv"
    
    [ ok_btn ,  cancel_btn ] <- mapM (xmlGetWidget glade castToButton) [
     "ok_btn", "cancel_btn"]
     
    selected_template <- (newIORef Nothing)
          
    return $ TemplateChooserDialog dialog_wnd ok_btn cancel_btn templates_tv selected_template



initTemplateChooser :: TemplateChooserDialog -> Session ->  IO ()
initTemplateChooser gui session = do
    let path = sessionPath session
    templates <- listTransactionTemplate path
    model <- listStoreNew templates
    
    initTreeViewColumns (templates_tv gui) model [
        ("Имя шаблона", transactionTemplateName  ),
        ("Назначение" , \t -> (take 40 (transactionTemplateReason t)))]
    
    
    bindTreeViewHandlers (\s t -> isPrefixOf s (transactionTemplateName t)) (updateTemplateData gui) (templates_tv gui) model

    
    
updateTemplateData :: TemplateChooserDialog -> TransactionTemplate -> IO ()
updateTemplateData gui str = do
    writeIORef   (selected_template gui) $ Just str  
    validateTemplateChooser gui
            
            

validateTemplateChooser :: TemplateChooserDialog -> IO ()
validateTemplateChooser gui = do
    let isSet = \getter -> isRefSet $ getter gui
    setButtonSensitive (ok_btn gui) =<< isSet selected_template
    

showTemplateChooser :: (WindowClass t_parent) => t_parent -> Session -> IO (Maybe TransactionTemplate)
showTemplateChooser parent session = do
    gui <- loadTemplateChooser "Resources/templateChooser_dialog.glade"
    windowSetTransientFor (dialog_wnd gui) parent 
    initTemplateChooser gui session
    validateTemplateChooser gui
    resp <- dialogRun (dialog_wnd gui)
    widgetDestroy  (dialog_wnd gui)
    
    if resp == ResponseOk
        then readIORef (selected_template gui)
        else return Nothing


module TemplateSaver where

import Data.IORef


-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- Common imports

-- Client imports
import GtkCommon




data TemplateSaverDialog = TemplateSaverDialog { dialog_wnd     :: Dialog
                                                   , ok_btn     :: Button
                                                   , cancel_btn :: Button
                                                   , name_ent   :: Entry 
                                                   , name       :: IORef (Maybe String)
                                                   }
                                                 
loadTemplateSaver :: FilePath -> IO TemplateSaverDialog
loadTemplateSaver gladePath = do
    Just glade <- xmlNew gladePath
  
    dialog_wnd <- xmlGetWidget glade castToDialog "dialog_wnd"
    name_ent   <- xmlGetWidget glade castToEntry  "name_ent"
    
    [ ok_btn ,  cancel_btn ] <- mapM (xmlGetWidget glade castToButton) [
     "ok_btn", "cancel_btn"]
     
    name <- newIORef Nothing
          
    return $ TemplateSaverDialog dialog_wnd ok_btn cancel_btn name_ent name



initTemplateSaver :: TemplateSaverDialog ->  IO ()
initTemplateSaver gui= do
    let entry = name_ent gui
    
    let onChange = do
        validateTemplateSaver gui
        return ()
    

    onClicked (ok_btn gui) $ do
        isValid <- validateTemplateSaver gui
        if isValid 
            then do
                str <- entryGetText (name_ent gui)
                writeIORef (name gui) (Just str)
                dialogResponse (dialog_wnd  gui) ResponseOk
            else return ()

    onClicked (cancel_btn gui) (dialogResponse (dialog_wnd  gui) ResponseCancel)
    return ()
   
            

validateTemplateSaver :: TemplateSaverDialog -> IO Bool
validateTemplateSaver gui = do
    str <- entryGetText (name_ent gui)
    let cond = not (null str)
    return cond
    

showTemplateSaver :: (WindowClass t_parent) => t_parent -> IO (Maybe String)
showTemplateSaver parent = do
    gui <- loadTemplateSaver "Resources/saveTemplate_dialog.glade"
    windowSetTransientFor (dialog_wnd gui) parent 
    initTemplateSaver gui 
    --validateTemplateSaver gui
    resp <- dialogRun (dialog_wnd gui)
    widgetDestroy     (dialog_wnd gui)
    
    if resp == ResponseOk
        then readIORef (name gui)
        else return Nothing

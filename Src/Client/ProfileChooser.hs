
module ProfileChooser where

import Data.IORef
import Control.Monad
import Data.List ( isPrefixOf, isSuffixOf)
import System.Directory

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- Common imports


-- Client imports
import ClientEntities
import DataModel
import GtkCommon



data ProfileChooserDialog = ProfileChooserDialog { dialog_wnd   :: Dialog
                                                 , ok_btn       :: Button
                                                 , close_btn    :: Button
                                                 , name_lbl     :: Label
                                                 , unp_lbl      :: Label
                                                 , date_lbl     :: Label
                                                 , profiles_tv  :: TreeView
                                                 , selected_path:: IORef (Maybe FilePath ) 
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
     
    setected_unp <- (newIORef Nothing)
          
    return $ ProfileChooserDialog dialog_wnd ok_btn close_btn name_lbl unp_lbl date_lbl profiles_tv setected_unp



initProfileChooser :: ProfileChooserDialog -> IO ()
initProfileChooser gui = do
    paths <- (getDirectoryContents "Profiles") 
    model <- listStoreNew $ map (\p -> ("Profiles/"++p, take (length p - 3) p)) (filter (isSuffixOf ".db") paths)
    
    initTreeViewColumns (profiles_tv gui) model [("Профили", snd) ]
    
    bindTreeViewHandlers (\str p -> isPrefixOf str (snd p)) (updateProfileData gui) (profiles_tv gui) model

    
    
updateProfileData :: ProfileChooserDialog -> (FilePath, String) -> IO ()
updateProfileData gui (path, str) = do
    writeIORef   (selected_path gui) $ Just path  
    profile <- findProfileByPath path
    renderProfileInfo (Just profile) (name_lbl gui) (unp_lbl gui) (date_lbl gui)
            
            
getSessionData :: ProfileChooserDialog -> IO (Maybe Session)
getSessionData gui = do
    mpath <- readIORef $ selected_path gui
    case mpath of
        Just path -> loadSessionByProfilePath path
        Nothing   -> return Nothing

    

showProfileChooser :: (Maybe Session -> IO ()) -> IO()
showProfileChooser handler = do
    gui <- loadProfileChooser "Resources/profileChooser_dialog.glade"
    initProfileChooser gui
    
    onResponse (dialog_wnd gui) (responceHandler gui)
    widgetShowAll (dialog_wnd gui)
    

        
    where responceHandler gui resp = do
            case resp of
                ResponseOk -> do
                    putStrLn "something -> handler"
                    ms <- getSessionData gui
                    widgetDestroy (dialog_wnd gui)
                    handler ms
                otherwise  -> do
                    putStrLn "Nothing-> handler"
                    widgetDestroy (dialog_wnd gui)
                    handler Nothing


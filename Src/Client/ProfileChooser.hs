
module ProfileChooser where

import Data.IORef
import Control.Monad
import Data.List ( isPrefixOf )
import System.Directory
import System.Time

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- Common imports
import Types
import Validation

-- Client imports
import ClientEntities
import DataModel
import GtkCommon



data ProfileChooserDialog = ProfileChooserDialog { dialog_wnd  :: Dialog
                                                 , ok_btn      :: Button
                                                 , close_btn   :: Button
                                                 , name_lbl    :: Label
                                                 , unp_lbl     :: Label
                                                 , date_lbl    :: Label
                                                 , profiles_tv :: TreeView
                                                 , selected_unp:: IORef (Maybe UNP ) 
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
    paths <- (getDirectoryContents "Profiles") -- >>= (filterM doesFileExist)
    putStrLn $ show paths 
    model <- listStoreNew (map str2unp $ filter isValidUnp paths)
    
    initTreeViewColumns (profiles_tv gui) model [("УНП профиля", unp2str)]
    
    let unpDoesMatch = \unp str -> str `isPrefixOf` unp2str unp
    
    bindTreeViewHandlers unpDoesMatch (findProfileByUNP >=> updateProfileData gui) (profiles_tv gui) model

    
    
updateProfileData :: ProfileChooserDialog -> Maybe Profile -> IO ()
updateProfileData gui mprofile = do
    case mprofile of 
        Just prof -> do
            writeIORef   (selected_unp gui) $ (Just    . profileUnp )   prof
            labelSetText (unp_lbl      gui) $ (unp2str . profileUnp )   prof
            labelSetText (name_lbl     gui) $  profileName              prof
            labelSetText (date_lbl     gui) $ (showDate. profileDate)   prof
        Nothing -> do
            writeIORef   (selected_unp gui) Nothing
            labelSetText (unp_lbl      gui) "N/A"
            labelSetText (name_lbl     gui) "N/A"
            labelSetText (date_lbl     gui) "N/A"
     where showDate d = (show $ ctYear d) ++ " " ++ (show $ ctMonth d) ++ " " ++ (show $ ctDay d)
            
            
getSessionData :: ProfileChooserDialog -> IO (Maybe Session)
getSessionData gui = do
    munp <- readIORef $ selected_unp gui
    case munp of
        Just unp -> loadSessionByUNP unp
        Nothing  -> return Nothing

    

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
                                            putStrLn "Nothing->handler"
                                            widgetDestroy (dialog_wnd gui)
                                            handler Nothing


import System.IO

-- Gtk imports
import Graphics.UI.Gtk

-- Common imports

-- Client imports
import MainWindow     (showMainWindow)
import ProfileChooser (showProfileChooser)

main :: IO ()
main = do
    initGUI
    showProfileChooser onSessionSelected 
    mainGUI
    putStrLn "MainGui" 
    
    where onSessionSelected msession = do
            case msession of
                Nothing -> do
                    putStrLn "handler>>>Nothing"
                    mainQuit
                Just session -> do
                    putStrLn "handler>>>Just"
                    showMainWindow session onMainResponse
                    where onMainResponse resp = do
                            putStrLn $ show resp
                            if resp == ResponseOk
                                then showProfileChooser onSessionSelected
                                else mainQuit 
    
    
    
                                    
     
        
            
        

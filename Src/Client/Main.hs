
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
    showProfileChooser handler 
    mainGUI
    putStrLn "MainGui" 
    
    
    
    where handler msession = do 
                    case msession of
                        Nothing -> do
                            putStrLn "handler>>>Nothing"
                            mainQuit
                        Just session -> do
                            putStrLn "handler>>>Just"
                            resp <- showMainWindow session
                            if resp == ResponseClose
                                then mainQuit 
                                else showProfileChooser handler
                                    
     
        
            
        

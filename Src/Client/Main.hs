
import System.IO

-- Gtk imports
import Graphics.UI.Gtk

-- Common imports

-- Client imports
import ClientEntities
import MainWindow  (showMainWindow)

main :: IO ()
main = do
    initGUI
    showMainWindow 
    mainGUI

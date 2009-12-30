
import System.IO

-- Gtk imports
import Graphics.UI.Gtk

-- Common imports
import Validation

-- Client imports
import ClientEntities
import MainWindow  (showMainWindow)

main :: IO ()
main = do
    initGUI
    showMainWindow $ Session $ str2unp "1234567890123"
    mainGUI

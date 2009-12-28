
import System.IO

-- Gtk imports
import Graphics.UI.Gtk
-- Client imports
import MainWindow  (showMainWindow)

main :: IO ()
main = do
    initGUI
    showMainWindow
    mainGUI

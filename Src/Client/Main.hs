
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

    where onSessionSelected msession =
            case msession of
                Nothing -> mainQuit
                Just session -> do
                    showMainWindow session onMainResponse
                    where onMainResponse resp =
                            if resp == ResponseOk
                                then showProfileChooser onSessionSelected
                                else mainQuit









module Main()
       where

import Network
import System.IO

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade ()

import Message
import TransactionDialog
import MainWindow

host = "127.0.0.1"
port = PortNumber 6555


                                  
                  

                                                          
testSend message = withSocketsDo $ do
	handle <- connectTo host port
	hPrint handle (show message)
	hClose handle
                                                  
testLogToConsole message= do
  print ("Send transaction: " ++ (show message))
  
  

  

main :: IO ()
main = do
	initGUI
  
	gui <- loadMainWindow "mainWindow.glade"
	onDestroy (window gui) mainQuit
	widgetShowAll (window gui)
	mainGUI

module ClientMessage where

import Network
import System.IO

-- Common imports
import Types ()
import Message



host = "127.0.0.1"
port = PortNumber 6555



testSend message = withSocketsDo $ do
    handle <- connectTo host port
    hPrint handle (show message)
    hClose handle
                                                  

    
testLogToConsole :: (Show a) => a -> IO ()
testLogToConsole message= do
  print ("Send transaction: " ++ (show message))
  
  
  
makeMessage :: String -> Request -> Message
makeMessage unp request = Message { senderId = ClientId unp
                                  , body = show request
                                  , digest = "0"
                                  }

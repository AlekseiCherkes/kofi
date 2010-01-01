module MessageHandler
    where

import Message
import System.IO
import System.Log.Logger

handleMessage :: String -> Handle -> IO ()
handleMessage name handle = do
  cnts <- hGetContents handle
  -- let msg = (read cnts) :: Message
  infoM "root.clients" $ "Message from" ++ ": " ++ name ++ ": " ++ cnts

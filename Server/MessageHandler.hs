module MessageHandler
    where

import Message
import System.IO

handleMessage :: String -> Handle -> IO ()
handleMessage name handle = do
  str <- (hGetContents handle)
  let msg = (read str) :: Message
  print $ "Message from" ++ ":" ++ name
  print $ str
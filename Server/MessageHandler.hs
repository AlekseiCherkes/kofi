module MessageHandler
    where

import Message

handleMessage :: String -> String -> IO ()
handleMessage name str = do
  let msg = (read str) :: Message
  print $ "Message from" ++ ":" ++ name
  print $ str
module MessageHandler
    where

import Message
import Crypto
import DataModel

import System.IO
import System.Log.Logger

handleMessage :: String -> Handle -> IO ()
handleMessage name handle = do
  cnts <- hGetContents handle
  
  infoM "root.client" $ "Message from" ++ ": " ++ name ++ ":"
  infoM "root.client" $ "Context: " ++ cnts
  
  let msg = (read cnts) :: Message
      
  infoM "root.client" $ "Message: " ++ (show msg)
  
  cmp <- findCompanyByUNP (unp $ senderId msg)
  
  infoM "root.client" $ "Company: " ++ (show cmp)

  -- let isValid = verifyMessage (serverRecvKey cmp) msg
  
  

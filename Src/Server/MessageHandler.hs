module MessageHandler
    where

import Message
import Crypto
import qualified DataModel as DM

import System.IO
import Data.Maybe
import System.Log.Logger

handleMessage :: String -> Handle -> IO ()
handleMessage name handle = do
  cnts <- hGetContents handle
  
  infoM "root.client" $ "Message from" ++ ": " ++ name ++ ":"
  infoM "root.client" $ "Context: " ++ cnts
  
  let msg = (read cnts) :: Message
      
  infoM "root.client" $ "Message: " ++ (show msg)
  
  cmp <- DM.findCompanyByUNP (unp $ senderId msg)
  
  infoM "root.client" $ "Company: " ++ (show cmp)

  let recvKey = DM.serverRecvKey $ fromJust cmp
  let sendKey = DM.serverSendKey $ fromJust cmp

  print recvKey
  print sendKey
  
  let isValid = verifyMessage recvKey msg
  print $ "isValid: " ++ (show isValid)
  
  let dmb = decodeMessageBody recvKey (body msg)
  print $ "Message body: " ++ dmb
  
  -- let ansver = "server responce"
  -- let rmsg = createMessage 

  
  

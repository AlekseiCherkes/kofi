module ClientMessage where

import Network
import System.IO

-- Common imports
import Types
import Message
import Crypto

-- Client imports
import ClientEntities



host = "127.0.0.1"
port = PortNumber 6555


sendRequest :: Session -> Request -> IO Response
sendRequest session request = do
  let unp        = (profileUnp .sessionProfile) session
  let sendRSAKey = ("MI8=","DQ==") --sessionSendKey session
  let recvRSAKey = ("R2s=","BV0=") --sessionRecvKey session
  
    
  let mb = (show request)  
  let emb = encodeMessageBody sendRSAKey mb
  let msg = createMessage sendRSAKey (ClientId unp) emb
       
  print $ "Message body: " ++ mb
  print $ "Encrypted message body: " ++ emb
  testSendAndRecv msg recvRSAKey

testSendAndRecv :: Message -> RSAKey -> IO Response
testSendAndRecv message recvRSAKey = do
  h <- connectTo host port
  hPutStrLn h (show message)
  hFlush h
  ret <- hGetLine h
  hClose h
  print $ "Response: " ++ ret
  let msg = read ret :: Message
  print $ "Msg: " ++ (show msg)
  let dmb = decodeMessageBody recvRSAKey $ body msg
  print $ "Decoded response: " ++ dmb
  return $ read dmb



  
  
  


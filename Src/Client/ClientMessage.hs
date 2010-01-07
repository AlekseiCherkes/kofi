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
    let sendRSAKey = sessionSendKey session
    let recvRSAKey = sessionRecvKey session
  
    
    let mb = (show request)  
    let emb = encodeMessageBody sendRSAKey mb
    let msg = createMessage sendRSAKey (ClientId unp) emb
  
    case request of
        CommitTransaction _ -> do
            sendOnly msg
            return Silence
        otherwise -> sendAndRecv msg recvRSAKey
            


sendOnly :: Message -> IO ()
sendOnly message = do
    h <- connectTo host port
    hPutStrLn h (show message)
    hFlush h
    hClose h

sendAndRecv :: Message -> RSAKey -> IO Response
sendAndRecv message recvRSAKey = do
    h <- connectTo host port
    hPutStrLn h (show message)
    hFlush h
    ret <- hGetLine h
    hClose h
    
    let msg = read ret :: Message
    let dmb = decodeMessageBody recvRSAKey $ body msg
    let mresp = read dmb
    case mresp of
        Nothing -> return $ Error "Server response cannot be parsed."
        Just r  -> return r


  
  


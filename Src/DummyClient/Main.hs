module Main
       where
       
import Types
import Message
import Crypto

import Network
import System.IO

--------------------------------------------------------------------------------
-- Messages
--------------------------------------------------------------------------------

sendRSAKey = ("+q6x","vw==")
recvRSAKey = ("ATKZxw==","WbJZ")
myUNP = "842902100"
apk1 = AccountPK "123456789" "000000001"
apk2 = AccountPK "987654321" "000000001"

testTransaction = CommitedTransaction { reason = "test this client server communication"
                                      , creditAccount = apk1
                                      , debitAccount  = apk2
                                      , amount = 100.0
                                      , priority = Normal
                                      }

msg_body = GetBalance apk1
-- body = CommitedTransaction testTransaction

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main = do
  let emb = encodeMessageBody sendRSAKey (show msg_body )
  let msg = createMessage sendRSAKey (ClientId myUNP) emb
  print "Message to send: "
  print msg
  testSend msg

--------------------------------------------------------------------------------
-- Network utilits
--------------------------------------------------------------------------------

host = "127.0.0.1"
port = PortNumber 6555

testSend message = withSocketsDo $ do
    handle <- connectTo host port
    hPrint handle (show message)
    hClose handle

testSendAndRecv message = withSocketsDo $ do
    handle <- connectTo host port
    hPrint handle (show message)
    (hGetContents handle >>= print)
    hClose handle

--------------------------------------------------------------------------------
-- End
--------------------------------------------------------------------------------

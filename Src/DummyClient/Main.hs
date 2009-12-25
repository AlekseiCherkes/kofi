module Main
       where
       
import Types
import Message
import Crypto

--------------------------------------------------------------------------------
-- Messages
--------------------------------------------------------------------------------

myRSAKey = ""
myUNP = "123456789"
apk1 = AccountPK "123456789" "000000001"
apk2 = AccountPK "987654321" "000000001"

testTransaction = CommitedTransaction { reason = "test this client server communication"
                                      , creditAccount = apk1
                                      , debitAccount  = apk2
                                      , amount = 100.0
                                      , priority = Normal
                                      }

body = GetBalance apk1
-- body = CommitedTransaction testTransaction

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main = do
  let msg = createMessage myRSAKey (ClientId myUNP) (show body)
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

testSendAndReceive message = withSocketsDo $ do
    handle <- connectTo host port
    hPrint handle (show message)
    hGetContent handle >> print
    hClose handle

--------------------------------------------------------------------------------
-- End
--------------------------------------------------------------------------------

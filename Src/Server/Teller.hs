module Teller
       where

import Prelude hiding (catch)

import Types

import System.Time
import System.Log.Logger
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan

data Transaction = Transaction { unp :: UNP
                               , payerAccPk :: AccountPK
                               , bnfcAccPk :: AccountPK
                               , amount :: Double
                               , reciveDate :: CalendarTime
                               , content :: String
                               , reason :: String }
                 deriving(Read, Show)
          
processTransaction :: Transaction -> IO ()
processTransaction tr = do
  infoM "server.teller" $ "Process transaction: " ++ (show tr)

startTeller transactionChannel channelName delay = forever $ do
  infoM "server.teller" $ "Starting teller thread (" ++ channelName ++ ")"
  tellerLoop `catch` handleException
  where  tellerLoop = forever $ do
           infoM "server.teller" $ "Process transaction channel: " ++ channelName
           isEmpty <- isEmptyChan transactionChannel
           if isEmpty
             then threadDelay delay
             else readChan transactionChannel >>= processTransaction
                  
         handleException (e::SomeException) = do
           errorM "server.teller" $ "Exception in teller thread (" ++ channelName ++ "): " ++ (show e)
module Teller
       where

import Types
import Message
import qualified DataModel as DM

import Data.Maybe
import System.Time
import qualified System.Log.Logger as Logger
import Control.Monad

import Control.Exception
import Control.Monad.Error
import Control.Concurrent
import Control.Concurrent.Chan

import Prelude hiding (catch)

--------------------------------------------------------------------------------
-- Logging utility functions
--------------------------------------------------------------------------------

infoM = Logger.infoM "root.teller"
errorM = Logger.errorM "root.teller"

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Transaction = Transaction { transactionUnp :: UNP
                               , transactionPayerAccPK :: AccountPK
                               , transactionBnfcAccPK :: AccountPK
                               , transactionAmount :: Double
                               , transactionReciveDate :: CalendarTime
                               , transactionContent :: String
                               , transactionReason :: String
                               , transactionPriority :: TransactionPriority
                               }
                 deriving(Read, Show)
          
--------------------------------------------------------------------------------
-- Checkers
--------------------------------------------------------------------------------

-- 0 "Success."
-- 1 "Invalid payer's BIC."
-- 2 "Invalid beneficiary's BIC."
-- 3 "Invalid payer's account."
-- 4 "Invalid beneficiary's account."
-- 5 "Payer hasn't this account."
-- 6 "Payer is closed already."
-- 7 "Beneficiary is closed already."
-- 8 "Payer's account is closed already."
-- 9 "Beneficiary's account is closed already."
-- 10 "Payer hasn't enough money for commited this transaction."

type ChekerMonad = ErrorT Int IO (Double, Double)

instance Error Int where
  noMsg = 0
  
retriveBank err bic = do
  bnk <- liftIO $ DM.findBankByBIC bic
  if (isJust bnk)
    then do
    liftIO $ infoM $ "Bank retrived: " ++ (show $ fromJust bnk)
    return $ fromJust bnk
    else do
    liftIO $ errorM $ "Can't retrive bank by BIC = " ++ (show bic)
    throwError err
  
retriveAcc err apk = do
  acc <- liftIO $ DM.findAccountByPK apk
  if (isJust acc)
    then do
    liftIO $ infoM $ "Account retrived: " ++ (show $ fromJust acc)
    return $ fromJust acc
    else do
    liftIO $ errorM $ "Can't retrive account by apk = " ++ (show apk)
    throwError err
                         
checkTransaction :: Transaction -> ChekerMonad
checkTransaction t = do
  pb <- retriveBank 1 $ bankBic $ transactionPayerAccPK t
  bb <- retriveBank 2 $ bankBic $ transactionPayerAccPK t
  pa <- retriveAcc 3 $ transactionPayerAccPK t
  ba <- retriveAcc 4 $ transactionBnfcAccPK t
  
  let pab = DM.accountBallance pa
  let bab = DM.accountBallance ba
  let delta = transactionAmount t

  if pab > delta 
    then return (pab - delta, bab + delta )
    else throwError 10

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

processTransaction :: Transaction -> IO ()
processTransaction tt = do
  infoM $ "Process transaction: " ++ (show tt)

  r <- runErrorT $ checkTransaction tt
  (status, payerBallance, bnfcBallance) <-
    case r of
      Right (p, b) -> do
        infoM "Transaction checked. Everething is OK." 
        return $ (0, Just p, Just b)
      Left s -> do
        errorM "Transaction checked. Thomething is BAD."  
        return $ (s, Nothing, Nothing)

  dummyDate <- toCalendarTime $ TOD 0 0
  
  let dmt = DM.Transaction 0 dummyDate
            (transactionReciveDate tt) status
            (transactionContent tt) (transactionReason tt)
            (transactionPayerAccPK tt) (transactionBnfcAccPK tt)
            (payerBallance) (bnfcBallance)
            (transactionAmount tt) (transactionPriority tt)
        
  DM.insertTransaction dmt
  
  if (isJust payerBallance) && (isJust bnfcBallance) 
    then DM.updateAccountBallance (transactionPayerAccPK tt) $ fromJust payerBallance
    else return ()

startTeller transactionChannel channelName delay = forever $ do
  infoM $ "Starting teller thread (" ++ channelName ++ ")"
  tellerLoop `catch` handleException
  where  tellerLoop = forever $ do
           infoM  $ "Process transaction channel: " ++ channelName
           isEmpty <- isEmptyChan transactionChannel
           if isEmpty
             then threadDelay delay
             else readChan transactionChannel >>= processTransaction
                  
         handleException (e::SomeException) = do
           errorM $ "Exception in teller thread (" ++ 
             channelName ++ "): " ++ (show e)
           
--------------------------------------------------------------------------------
-- End
--------------------------------------------------------------------------------

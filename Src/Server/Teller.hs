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

data Transaction = Transaction { transactionUNP :: UNP
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
-- 1 "System error.
-- 2 "Invalid payer's BIC."
-- 3 "Invalid beneficiary's BIC."
-- 4 "Invalid payer's account."
-- 5 "Invalid beneficiary's account."
-- 6 "Payer hasn't this account."
-- 7 "Payer is closed already."
-- 8 "Beneficiary is closed already."
-- 9 "Payer's account is closed already."
-- 10 "Beneficiary's account is closed already."
-- 11 "Coincidence of payer and beneficiary's accounts."
-- 12 "Payer hasn't enough money for commited this transaction."

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

retriveCmp err unp = do
  cmp <- liftIO $ DM.findCompanyByUNP unp
  if (isJust cmp)
    then do
    liftIO $ infoM $ "Company retrived: " ++ (show $ fromJust cmp)
    return $ fromJust cmp
    else do
    liftIO $ errorM $ "Can't retrive company by unp = " ++ (show unp)
    throwError err

checkDate err field = do
  if (isNothing $ field)
    then do
    liftIO $ infoM $ "Entity is opened."
    else do
    liftIO $ errorM $ "Entity closed. Close date: " ++ (show field)
    throwError err

checkAccountOwner err acc cmp = do
  if ((DM.accountOwnerUnp acc) == (DM.companyUnp cmp)) 
    then do
    liftIO $ infoM "Account belongs to author of the reqest."
    else do
    liftIO $ errorM $ "Author of the request hasn't have responsed account."
    throwError err

checkTransaction :: Transaction -> ChekerMonad
checkTransaction t = do
  pb <- retriveBank 2 $ bankBic $ transactionPayerAccPK t
  bb <- retriveBank 3 $ bankBic $ transactionPayerAccPK t
  pa <- retriveAcc 4 $ transactionPayerAccPK t
  ba <- retriveAcc 5 $ transactionBnfcAccPK t

  pc <- retriveCmp 1 $ DM.accountOwnerUnp pa
  bc <- retriveCmp 1 $ DM.accountOwnerUnp ba

  checkAccountOwner 6 pa pc

  checkDate 7 $ DM.companyUnregistryDate pc
  checkDate 8 $ DM.companyUnregistryDate bc  
  checkDate  9 $ DM.accountCloseDate pa 
  checkDate 10 $ DM.accountCloseDate ba

  if (DM.accountPK pa) == (DM.accountPK ba)
    then throwError 11
    else liftIO $ return ()

  let pab = DM.accountBallance pa
  let bab = DM.accountBallance ba
  let delta = transactionAmount t

  if pab >= delta 
    then return (pab - delta, bab + delta )
    else throwError 12

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
        infoM $ "Transaction checked. Everething is OK." 
        return $ (0, Just p, Just b)
      Left s -> do
        errorM $ "Transaction checked. Thomething is BAD. Error code: " ++ (show s)
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
    then do
    DM.updateAccountBallance (transactionPayerAccPK tt) $ fromJust payerBallance
    DM.updateAccountBallance (transactionBnfcAccPK tt) $ fromJust bnfcBallance
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

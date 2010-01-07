module DataModel
    where

import Types
import Message

import Data.List
import Data.Maybe
import System.Time
-- import Database.HSQL
import Database.HSQL.SQLite3
import Control.Exception
import qualified System.Log.Logger as Logger

import System.IO

--------------------------------------------------------------------------------
-- Logging utility functions
--------------------------------------------------------------------------------

infoM = Logger.infoM "root.db"
errorM = Logger.errorM "root.db"

--------------------------------------------------------------------------------
-- Utiliy functions
--------------------------------------------------------------------------------

catchQueries = catchSql

fromSqlMaybe t (Just v) = fromJust $ fromSqlValue t v
fromSqlMaybe _ Nothing = Nothing

fromSqlClock v = do
  let clockT = fromJust $ fromSqlValue (SqlTimeStamp) v
  calendarT <- toCalendarTime clockT
  return calendarT

fromSqlClockMaybe Nothing = return Nothing
fromSqlClockMaybe (Just v) = do
  let clockT = fromJust $ fromSqlValue (SqlTimeStamp) v
  calendarT <- toCalendarTime clockT
  return (Just calendarT)
  
formatValues values = foldl1 (++) $ intersperse ", " values

unpToSql unp = toSqlValue $ unp2str unp
bicToSql bic = toSqlValue $ bic2str bic
accToSql acc = toSqlValue $ acc2str acc
amountToSql amount = toSqlValue amount

priority2sql Normal = toSqlValue $ show 0
priority2sql Urgent = toSqlValue $ show 1

sql2priority str = 
  case (read str :: Int) of
    0 -> Normal
    1 -> Urgent
    _ -> error "Can't read transaction priority level from database."

--------------------------------------------------------------------------------
-- Common functions
--------------------------------------------------------------------------------

withDB :: FilePath -> IOMode -> (Connection -> IO a) -> IO a
withDB path mode = bracket
                   (connect path mode)
                   (\conection -> disconnect conection)

withServerDB = withDB "server.db" ReadWriteMode
withManualDB = withDB "manual.db" ReadMode

sqlExec connector command = do
  infoM $ "Executing: " ++ command
  catchSql 
    (connector $ \conn -> execute conn command)
    (\e -> errorM $ show e)

sqlQueryList connector fetcher q = do
  infoM $ "Query list: " ++ q
  catchSql perform handle
    where perform = connector $ \conn -> do
            result <- (query conn q >>= collectRows fetcher)
            infoM $ "Result: " ++ (show result)
            return (Just result)
          handle e = (errorM $ show e) >> return Nothing

sqlQueryRec connector fetcher q = do
  infoM $ "Query record: " ++ q
  catchSql perform handler
  where perform = connector $ \conn -> do
          result <- (query conn q >>= collectRows fetcher)
          infoM $ "Result: " ++ (show result)        
          if (length result) == 1
            then return (Just (head result))
            else return Nothing
        handler e = (errorM $ show e) >> return Nothing

--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

data Company = Company { companyUnp :: UNP
                       , companyName :: String
                       , companyRegistryDate :: CalendarTime
                       , companyUnregistryDate :: Maybe CalendarTime
                       , companyServerRecvKey :: RSAKey
                       , companyServerSendKey :: RSAKey
                       , companyClientRecvKey :: RSAKey
                       , companyClientSendKey :: RSAKey
                       }
               deriving (Read, Show)
                        
data Account = Account { accountPK :: AccountPK
                       , accountOwnerUnp :: UNP
                       , accountBallance :: Double
                       , accountOpenDate :: CalendarTime
                       , accountCloseDate :: Maybe CalendarTime
                       }               
               deriving (Read, Show)
                        
data Transaction = Transaction { transactionId :: Int -- may be 0 while insertion
                               , transactionCommitDate :: CalendarTime -- hasn't using while insertion
                               , transactionReciveDate :: CalendarTime
                               , transactionStatusId :: Int
                               , transactionContent :: String
                               , transactionReason :: String
                               , transactionPayerAccountPK :: AccountPK
                               , transactionBnfcAccountPK :: AccountPK
                               , transactionPayerFinalBalance :: Maybe Double
                               , transactionBnfcFinalBalance :: Maybe Double
                               , transactionAmount :: Double
                               , transactionPriority :: TransactionPriority
                               }
                 deriving (Read, Show)                        

data Bank = Bank { bankBranchBic :: BIC
                 , bankBankBic :: String
                 , bankName :: String 
                 }
          deriving (Read, Show)
                   
--------------------------------------------------------------------------------
-- Fetchers
--------------------------------------------------------------------------------

fetchCompany :: Statement -> IO Company
fetchCompany stmt = do
  let get = getFieldValue stmt
  let getMB = getFieldValueMB stmt

  fvUnp <- get "company_unp"
  fvName <- get "company_name"
  fvRegDate <- get "registry_date"
  fvUnregDate <- getMB "unregistry_date" 
  fvServerRecvKey <- get "server_recv_key"
  fvServerSendKey <- get "server_send_key"
  fvClientRecvKey <- get "client_recv_key"
  fvClientSendKey <- get "client_send_key"  
  
  let unp = str2unp $ fromJust $ fromSqlValue (SqlChar 13) fvUnp 
  let name = fromJust $ fromSqlValue (SqlVarChar 256) fvName
  regDate <- toCalendarTime $ fromJust $ fromSqlValue (SqlDateTime) fvRegDate
  unregDate <- case fvUnregDate of 
    Just ud -> do
      r <- toCalendarTime $ fromJust $ fromSqlValue (SqlDateTime) ud 
      return $ Just r
    Nothing -> return Nothing
  let serverRecvKey = read $ fromJust $ fromSqlValue (SqlVarChar 1024) fvServerRecvKey
  let serverSendKey = read $ fromJust $ fromSqlValue (SqlVarChar 1024) fvServerSendKey
  let clientRecvKey = read $ fromJust $ fromSqlValue (SqlVarChar 1024) fvClientRecvKey
  let clientSendKey = read $ fromJust $ fromSqlValue (SqlVarChar 1024) fvClientSendKey

  return $ Company unp name 
    regDate unregDate
    serverRecvKey serverSendKey 
    clientRecvKey clientSendKey
    
fetchAccount stmt = do
  let get = getFieldValue stmt
  let getMB = getFieldValueMB stmt

  fvAccId <- get "acc_id"
  fvBankBic <- get "bank_bic"
  fvOwnerUnp <- get "owner_unp"
  fvBallance <- get "ballance"
  fvOpenDate <- get "open_date"
  fvCloseDate <- getMB "close_date"

  let accId = str2acc $ fromJust $ fromSqlValue (SqlChar 13) fvAccId
  let bankBic = str2bic $ fromJust $ fromSqlValue (SqlChar 9) fvBankBic
  let ownerUnp = str2unp $ fromJust $ fromSqlValue (SqlChar 13) fvOwnerUnp
  let ballance = fromJust $ fromSqlValue SqlMoney fvBallance
  openDate <- toCalendarTime $ fromJust $ fromSqlValue (SqlDateTime) fvOpenDate
  closeDate <- case fvCloseDate of 
    Just cd -> do
      r <- toCalendarTime $ fromJust $ fromSqlValue (SqlDateTime) cd
      return $ Just r
    Nothing -> return Nothing
    
  return $ Account (AccountPK accId bankBic) ownerUnp 
    ballance 
    openDate closeDate  
  
fetchBank stmt = do
  let get = getFieldValue stmt
  
  fvBranchBic <- get "branch_bic"
  fvBankBic <- get "bank_bic"
  fvName <- get "name"
  
  let branchBic = str2bic $ fromJust $ fromSqlValue (SqlChar 9) fvBranchBic
  let bankBic = fromJust $ fromSqlValue (SqlChar 3) fvBankBic
  let name = fromJust $ fromSqlValue (SqlVarChar 200) fvName
      
  return $ Bank branchBic bankBic name
      
fetchTransaction stmt = do
  let get = getFieldValue stmt
  let getMB = getFieldValueMB stmt

  fvId <- get "trn_id"                        
  fvCommitDate <- get "commit_date"
  fvReciveDate <- get "recive_date"
  fvStatusId <- get "status_id"
  fvContent <- get "content"
  fvReason <- get "reason"
  fvPayerAccId <- get "payer_acc_id"
  fvPayerBankBic <- get "payer_bank_bic"
  fvBnfcAccId <- get "bnfc_acc_id"
  fvBnfcBankBic <- get "bnfc_bank_bic"
  fvPayerFinalBalance <- getMB "payer_final_balance"
  fvBnfcFinalBalance <- getMB "bnfc_final_balance"
  fvAmount <- get "amount"
  fvPriority <- get "priority"
  
  let xid = fromJust $ fromSqlValue SqlInteger fvId
  commitDate <- toCalendarTime $ fromJust $ fromSqlValue (SqlDateTime) fvCommitDate
  reciveDate <- toCalendarTime $ fromJust $ fromSqlValue (SqlDateTime) fvReciveDate
      
  let statusId = fromJust $ fromSqlValue SqlInteger fvStatusId
  let content = fromJust $ fromSqlValue (SqlVarChar 4000) fvContent
  let reason = fromJust $ fromSqlValue (SqlVarChar 1000) fvReason
  let payerAccId = str2acc $ fromJust $ fromSqlValue (SqlChar 13) fvPayerAccId
  let payerBankBic = str2bic $ fromJust $ fromSqlValue (SqlChar 9) fvPayerBankBic
  let bnfcAccId = str2acc $ fromJust $ fromSqlValue (SqlChar 13) fvBnfcAccId
  let bnfcBankBic = str2bic $ fromJust $ fromSqlValue (SqlChar 9) fvBnfcBankBic
      
  let payerFinalBalance = case fvPayerFinalBalance of  
        Just a -> fromJust $ fromSqlValue SqlMoney a
        Nothing -> Nothing
        
  let bnfcFinalBalance = case fvPayerFinalBalance of  
        Just a -> fromJust $ fromSqlValue SqlMoney a
        Nothing -> Nothing
        
  let amount = fromJust $ fromSqlValue SqlMoney fvAmount
  let priority = sql2priority $ fromJust $ fromSqlValue SqlInteger fvPriority
                          
  return $ Transaction xid commitDate reciveDate 
    statusId content reason 
    (AccountPK payerAccId payerBankBic) (AccountPK bnfcAccId bnfcBankBic)
    payerFinalBalance bnfcFinalBalance amount 
    priority
    
fetchTransactionStatus stmt = do
  fvMessage <- getFieldValue stmt "message"
  let msg = fromJust $ fromSqlValue (SqlVarChar 256) fvMessage
  return msg

--------------------------------------------------------------------------------
-- Companies
-------------------------------------------------------------------------------- 

insertCompany company = sqlExec withServerDB cmd
  where cmd = "INSERT INTO Company VALUES (" ++ values ++");"
        values = formatValues [ toSqlValue $ unp2str $ companyUnp company
                              , toSqlValue $ companyName company
                              , toSqlValue $ toClockTime (companyRegistryDate company)
                              , clockValue $ companyUnregistryDate company
                              , toSqlValue $ show $ companyServerRecvKey company 
                              , toSqlValue $ show $ companyServerSendKey company
                              , toSqlValue $ show $ companyClientRecvKey company 
                              , toSqlValue $ show $ companyClientSendKey company ]
          where clockValue (Just a) = toSqlValue $ toClockTime a
                clockValue Nothing = "NULL"

findCompanyByUNP unp = sqlQueryRec withServerDB fetchCompany q
  where q = "SELECT * FROM Company " ++
            "WHERE company_unp = " ++ (unpToSql unp) ++ ";"

--------------------------------------------------------------------------------
-- Accounts
--------------------------------------------------------------------------------

insertAccount account = sqlExec withServerDB cmd
  where cmd = "INSERT INTO Account VALUES(" ++ values ++");"
        values = formatValues [ toSqlValue $ acc2str $ accId $ accountPK account
                              , toSqlValue $ bic2str $ bankBic $ accountPK account
                              , toSqlValue $ unp2str $ accountOwnerUnp account
                              , toSqlValue $ accountBallance account
                              , toSqlValue $ toClockTime (accountOpenDate account)
                              , clockValue $ accountCloseDate account ]
          where clockValue (Just a) = toSqlValue $ toClockTime a
                clockValue Nothing = "NULL"                 

findAccountByPK apk = sqlQueryRec withServerDB fetchAccount q
  where q = "SELECT * FROM Account " ++
            "WHERE acc_id = " ++ (accToSql $ accId apk) ++ 
            "AND bank_bic = " ++ (bicToSql $ bankBic apk) ++ ";"
            
updateAccountBallance accountPK ballance = sqlExec withServerDB cmd
  where cmd = "UPDATE Account " ++ 
              "SET ballance = " ++ (toSqlValue $ show ballance) ++ " " ++
              "WHERE acc_id = " ++ (accToSql $ accId  accountPK) ++ " " ++
              "AND bank_bic = " ++ (bicToSql $ bankBic accountPK) ++ ";"

--------------------------------------------------------------------------------
-- Bank
--------------------------------------------------------------------------------

findBankByBIC bic = sqlQueryRec withManualDB fetchBank q
  where q = "SELECT * FROM Branch " ++
            "WHERE branch_bic = " ++ (bicToSql $ bic) ++ ";"

--------------------------------------------------------------------------------
-- Transactions
--------------------------------------------------------------------------------

insertTransaction t = sqlExec withServerDB cmd
  where cmd = "INSERT INTO CommitedTransaction VALUES(" ++ values ++ ");"
        values = formatValues [ "NULL" -- autoincremented primary key
                              , "current_timestamp" -- commit date
                              , toSqlValue $ toClockTime (transactionReciveDate t)
                              , toSqlValue $ transactionStatusId t
                              , toSqlValue $ transactionContent t
                              , toSqlValue $ transactionReason t
                              , toSqlValue $ acc2str $ accId $ transactionPayerAccountPK t
                              , toSqlValue $ bic2str $ bankBic $ transactionPayerAccountPK t
                              , toSqlValue $ acc2str $ accId $ transactionBnfcAccountPK t
                              , toSqlValue $ bic2str $ bankBic $ transactionBnfcAccountPK t
                              , toSqlValue $ transactionPayerFinalBalance t
                              , toSqlValue $ transactionBnfcFinalBalance t
                              , toSqlValue $ transactionAmount t
                              , toSqlValue $ priority2sql $ transactionPriority t]
                 
findTransactionsForStatement apk from to = sqlQueryList withServerDB fetchTransaction q
  where q = "SELECT * FROM CommitedTransaction " ++
            "WHERE commit_date BETWEEN " ++
             fd ++ " AND " ++ td ++ " " ++
             "AND status_id = 0;"
             where fd = toSqlValue $ toClockTime from
                   td = toSqlValue $ toClockTime to

findTransactionsForLog apk from to = sqlQueryList withServerDB fetchTransaction q
  where q = "SELECT * FROM CommitedTransaction " ++
            "WHERE commit_date BETWEEN " ++
             fd ++ " AND " ++ td ++ ";"
             where fd = toSqlValue $ toClockTime from
                   td = toSqlValue $ toClockTime to
                   
findTransactionStatusMessageById id = sqlQueryRec withServerDB fetchTransactionStatus q
  where q = "SELECT message FROM Status " ++
            "WHERE status_id = " ++ (toSqlValue id) ++ ";"

--------------------------------------------------------------------------------
-- End
--------------------------------------------------------------------------------
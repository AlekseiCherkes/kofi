module DataModel
    where

import Types
import Crypto

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
  
  print fvAccId
  print fvBankBic
  print fvOwnerUnp
  print fvBallance
  print fvOpenDate
  print fvCloseDate
    
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

--------------------------------------------------------------------------------
-- Bank
--------------------------------------------------------------------------------

findBankByBIC bic = sqlQueryRec withManualDB fetchBank q
  where q = "SELECT * FROM Branch " ++
            "WHERE branch_bic = " ++ (bicToSql $ bic) ++ ";"

--------------------------------------------------------------------------------
-- Transactions
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- End
--------------------------------------------------------------------------------
                
-- все Maybe
  
-- insertAccount
-- updateAccount

-- insertCompany
-- updateCompany

-- findCompanyByUNP
-- findAccountByACC
-- findCompanyByACC

-- insertTransaction
                
-- findTransactionsForStatement ACC From To
-- findTransactionsForLog ACC From To





module DataModel where


import System.IO
import System.Time

import Types
import qualified ClientEntities as E



import Data.Maybe
import Data.List
import Data.String.UTF8 ()
import Control.Exception
import Database.HSQL.SQLite3

--------------------------------------------------------------------------------
-- Connection functions
--------------------------------------------------------------------------------

banksManualFilePath = "manual.db"

withDB :: FilePath -> (Connection -> IO a) -> IO a
withDB filePath = bracket
                  (connect filePath ReadWriteMode)
                  (\conection -> disconnect conection)

withBanksManual :: (Connection -> IO a) -> IO a
withBanksManual = bracket
                  (connect banksManualFilePath ReadMode)
                  (\conection -> disconnect conection)

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

sqlHandleError :: SqlError -> IO [a]
sqlHandleError e = do
  print $ "SQL Error: " ++ (show e)
  -- throwIO (ErrorCall $ "SQL Error: " ++ (show e))
  return []

sqlPerformQuery fetchRowFunction q connection =
  print q >>
  query connection q >>=
  collectRows fetchRowFunction >>=
  return

sqlQuery connectFunction
  fetchRowFunction
  sqlQuery = catchSql
             (connectFunction (sqlPerformQuery fetchRowFunction sqlQuery ))
             sqlHandleError

sqlQueryGetFirst q = do
  results <- q
  return $ head results
  
formatValues values = foldl1 (++) $ intersperse ", " values

--------------------------------------------------------------------------------
-- Fetch row functions
--------------------------------------------------------------------------------

fetchCompany :: Statement -> IO E.Company
fetchCompany stmt = do
  fvName <- getFieldValue stmt "company_name"
  fvUnp <- getFieldValue stmt "company_unp"
  let name = fromJust $ fromSqlValue (SqlVarChar 256) fvName
  let unp = str2unp $ fromJust $ fromSqlValue (SqlChar 13) fvUnp
  return $ E.Company name unp

fetchBankBranch :: Statement -> IO E.Bank
fetchBankBranch stmt = do
  fvName <- getFieldValue stmt "name"
  fvBic <- getFieldValue stmt "branch_bic"
  let name = fromJust $ fromSqlValue (SqlVarChar 200) fvName
  let bic = str2bic $ fromJust $ fromSqlValue (SqlVarChar 9) fvBic
  return $ E.Bank name bic

fetchAccount :: Statement -> IO E.Account
fetchAccount stmt = do
  fvAcc <- getFieldValue stmt "acc_id"
  fvUnp <- getFieldValue stmt "company_unp"
  fvBic <- getFieldValue stmt "bank_bic"
  let acc = str2acc $ fromJust $ fromSqlValue (SqlChar 13) fvAcc
  let unp = str2unp $ fromJust $ fromSqlValue (SqlChar 9) fvUnp
  let bic = str2bic $ fromJust $ fromSqlValue (SqlChar 13) fvBic
  let payerAcc = E.Account (AccountPK acc bic) unp
  return payerAcc
  
fetchBic :: Statement -> IO BIC
fetchBic stmt = (return . str2bic . fromJust . fromSqlValue (SqlChar 13)) =<< (getFieldValue stmt "bank_bic")

fetchProfile :: Statement -> IO E.Profile
fetchProfile stmt = do
  fvUnp <- getFieldValue stmt "unp"
  fvName <- getFieldValue stmt "name"
  fvDate <- getFieldValue stmt "date"

  let unp = str2unp $ fromJust $ fromSqlValue (SqlChar 13) fvUnp
  let name = fromJust $ fromSqlValue (SqlVarChar 256) fvName
  date <- toCalendarTime $ fromJust $ fromSqlValue (SqlDateTime) fvDate

  return $ E.Profile unp name date

fetchKeys :: Statement -> IO (RSAKey, RSAKey)
fetchKeys stmt = do
  fvRecvKey <- getFieldValue stmt "recv_key"
  fvSendKey <- getFieldValue stmt "send_key"

  let recvKey = read $ fromJust $ fromSqlValue (SqlVarChar 1024) fvRecvKey
  let sendKey = read $ fromJust $ fromSqlValue (SqlVarChar 1024) fvSendKey

  return $ (recvKey, sendKey)

fetchStatement :: Statement -> IO E.Statement 
fetchStatement stmt = do
  fvId <- getFieldValue stmt "statement_id"
  fvStartDate <- getFieldValue stmt "start_date"
  fvEndDate <- getFieldValue stmt "end_date"
  fvAccId <- getFieldValue stmt "acc_id"
  fvBankBic <- getFieldValue stmt "bank_bic"
  fvStatementText <- getFieldValue stmt "statement_text"

  let xid = read $ fromJust $ fromSqlValue (SqlInteger) fvId
  startDate <- toCalendarTime $ fromJust $ fromSqlValue (SqlDateTime) fvStartDate
  endDate <- toCalendarTime $ fromJust $ fromSqlValue (SqlDateTime) fvEndDate
  let accId = read $ fromJust $ fromSqlValue (SqlChar 13) fvAccId
  let bankBic = read $ fromJust $ fromSqlValue (SqlChar 9) fvBankBic
  let text = fromJust $ fromSqlValue (SqlVarChar 2000) fvStatementText
      
  return $ E.Statement xid startDate endDate
    (AccountPK accId bankBic) text

fetchTransactionTemplate stmt = do
  fvTransactionTemplateId <- getFieldValue stmt "transaction_template_id" 
  fvTmplName <- getFieldValue stmt "tmpl_name" 
  fvPayerBankBic <- getFieldValue stmt "payer_bank_bic" 
  fvPayerAccId <- getFieldValue stmt "payer_acc_id" 
  fvBnfcBankBic <- getFieldValue stmt "bnfc_bank_bic" 
  fvBnfcAccId <- getFieldValue stmt "bnfc_acc_id" 
  fvAmount <- getFieldValue stmt "amount" 
  fvReason <- getFieldValue stmt "varchar"
  fvIsUrgent <- getFieldValue stmt "is_urgent" 
  
  let xid = read $ fromJust $ fromSqlValue (SqlInteger) fvTransactionTemplateId
  let name = fromJust $ fromSqlValue (SqlVarChar 16) fvTmplName
  let payerBankBic = read $ fromJust $ fromSqlValue (SqlChar 9) fvPayerBankBic
  let payerAccId = read $ fromJust $ fromSqlValue (SqlChar 13) fvPayerAccId
  let bnfcBankBic = read $ fromJust $ fromSqlValue (SqlChar 9) fvBnfcBankBic
  let bnfcAccId = read $ fromJust $ fromSqlValue (SqlChar 13) fvBnfcAccId
  let amount = read $ fromJust $ fromSqlValue (SqlMoney) fvAmount
  let reason = fromJust $ fromSqlValue (SqlVarChar 256) fvReason
  let isUrgent = read $ fromJust $ fromSqlValue (SqlBit) fvIsUrgent
  
  return $ E.TransactionTemplate xid name 
    (AccountPK payerBankBic payerAccId)
    (AccountPK bnfcBankBic bnfcAccId)
    amount reason isUrgent
    
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Access functions
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Profiles
--------------------------------------------------------------------------------   
findProfileByPath :: FilePath -> IO E.Profile
findProfileByPath file = sqlQueryGetFirst $
                         sqlQuery (withDB file) fetchProfile $
                         "SELECT * FROM Config;"

loadSessionByProfilePath :: FilePath -> IO (Maybe E.Session)
loadSessionByProfilePath file = do 
  profile <- findProfileByPath file
  (recvKey, sendKey) <- sqlQueryGetFirst $
                       sqlQuery (withDB file) fetchKeys  $
                       "SELECT * FROM Config;"

  return $ Just (E.Session profile file recvKey sendKey)


--------------------------------------------------------------------------------
-- Companies
--------------------------------------------------------------------------------
findCompanyByName :: FilePath -> Name -> IO E.Company
findCompanyByName file name = sqlQueryGetFirst $
                            sqlQuery (withDB file) fetchCompany  $
                            "SELECT * FROM Company " ++
                            "WHERE company_name = \"" ++ name ++ "\";"

findCompanyByUnp :: FilePath -> UNP -> IO E.Company
findCompanyByUnp file unp = sqlQueryGetFirst $
                          sqlQuery (withDB file) fetchCompany  $
                          "SELECT * FROM Company " ++
                          "WHERE company_unp = " ++ unpValue ++ ";"
                            where unpValue = (toSqlValue . unp2str) unp

findCompaniesByBank :: FilePath -> BIC -> IO [E.Company]
findCompaniesByBank file bic = sqlQuery (withDB file) fetchCompany $
                               "SELECT Company.* " ++
                               "FROM Company " ++
                               "INNER JOIN Account on " ++
                               "Account.company_unp = Company.company_unp " ++
                               "WHERE Account.bank_bic = " ++ bicValue ++";"
                                 where bicValue = (toSqlValue . bic2str) bic
                                 
fetchAllCompanies :: FilePath -> IO [E.Company]
fetchAllCompanies file = sqlQuery (withDB file) fetchCompany $
                               "SELECT * FROM Company;"
--------------------------------------------------------------------------------
-- Banks
--------------------------------------------------------------------------------

findBankByName :: Name -> IO E.Bank
findBankByName name = sqlQueryGetFirst $
                      sqlQuery withBanksManual fetchBankBranch  $
                      "SELECT * FROM Branch " ++
                      "WHERE name = \"" ++ name ++ "\";"

findBankByBic :: BIC -> IO E.Bank
findBankByBic bic = sqlQueryGetFirst $
                    sqlQuery withBanksManual fetchBankBranch  $
                    "SELECT * FROM Branch " ++
                    "WHERE branch_bic = " ++ bicValue ++ ";"
                      where bicValue = (toSqlValue . bic2str) bic

                            
findBanksByCompany :: FilePath -> UNP -> IO [E.Bank]
findBanksByCompany file unp = fetchBanksByBics =<< fetchBicsByCompany
  where
    fetchBicsByCompany = sqlQuery (withDB file) fetchBic $
                             "SELECT DISTINCT bank_bic " ++
                             "FROM Account " ++
                             "WHERE company_unp = " ++ unpValue ++ ";"
                               where unpValue = (toSqlValue . unp2str) unp

    fetchBanksByBics bics = sqlQuery withBanksManual fetchBankBranch $
                            "SELECT * " ++
                            "FROM Branch " ++
                            "WHERE branch_bic IN (" ++ bicsValue ++ ");"
                              where bicsValue = foldl1 (++) $ intersperse ", " $
                                                map (toSqlValue . bic2str) bics
                                                
                                                

listBanks :: IO [E.Bank]
listBanks = sqlQuery withBanksManual fetchBankBranch
            "SELECT * FROM Branch;"

--------------------------------------------------------------------------------
-- Accounts
--------------------------------------------------------------------------------

findAccountsByCompany :: FilePath -> UNP -> IO [E.Account]
findAccountsByCompany file unp = sqlQuery (withDB file) fetchAccount $
                                 "SELECT * FROM Account " ++
                                 "WHERE company_unp = " ++ unpValue ++ ";"
                                   where unpValue = (toSqlValue . unp2str) unp

findAccountsByBank :: FilePath -> BIC -> IO [E.Account]
findAccountsByBank file bic = sqlQuery (withDB file) fetchAccount $
                              "SELECT * FROM Account " ++
                              "WHERE bank_bic = " ++ bicValue ++ ";"
                                where bicValue = (toSqlValue .bic2str) bic
                                
findAccountsByCompanyAndBank :: FilePath -> UNP -> BIC -> IO [E.Account]
findAccountsByCompanyAndBank file unp bic = sqlQuery (withDB file) fetchAccount $
                              "SELECT * FROM Account " ++
                              "WHERE company_unp = " ++ unpValue ++ " " ++
                              "AND bank_bic = " ++ bicValue ++ ";"
                                where bicValue = (toSqlValue .bic2str) bic 
                                      unpValue = (toSqlValue . unp2str) unp                                

findAccount :: FilePath -> BIC -> ACC -> IO E.Account
findAccount file bic acc = sqlQueryGetFirst $
                           sqlQuery (withDB file) fetchAccount $
                           "SELECT * FROM Account " ++
                           "WHERE acc_id = " ++ accValue ++ " " ++
                           "AND bank_bic = " ++ bicValue ++ ";"
                             where accValue = (toSqlValue . acc2str) acc
                                   bicValue = (toSqlValue . bic2str) bic

--------------------------------------------------------------------------------
-- Counterparties
--------------------------------------------------------------------------------

listCounterparties :: FilePath -> IO [E.Company]
listCounterparties file = sqlQuery (withDB file) fetchCompany $
                          "SELECT * FROM Company;"

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

-- not tested !!!
insertStatement :: FilePath -> E.Statement -> IO ()
insertStatement file s = do
  -- print $ show cmd
  catchSql 
    (withDB file $ \conn -> execute conn cmd)
    (\e -> print $ show e)
  where cmd = "INSERT INTO Statement VALUES(" ++ values ++ ");"
        values = formatValues [ "NULL" -- autoincremented primary key
                              , toSqlValue $ toClockTime $ E.statementStartDate s
                              , toSqlValue $ toClockTime $ E.statementEndDate s
                              , toSqlValue $ acc2str $ accId $ E.statementAccountPK s
                              , toSqlValue $ bic2str $ bankBic $ E.statementAccountPK s
                              , toSqlValue $ E.statementText s
                              ]
                 
-- not tested !!!  
findStatementById :: FilePath -> Int -> IO E.Statement
findStatementById file xid = sqlQueryGetFirst $
                             sqlQuery (withDB file) fetchStatement $
                             "SELECT * FROM Statement " ++
                             "WHERE statement_id = " ++ idValue ++ ";"
                               where idValue = toSqlValue xid
                    
-- not tested !!!
listStatements :: FilePath -> IO [E.Statement]
listStatements file = sqlQuery (withDB file) fetchStatement $
                      "SELECT * FROM Statement;"

--------------------------------------------------------------------------------
-- Transaction templates
--------------------------------------------------------------------------------

-- not tested !!!  
insertTransactionTemplate :: FilePath -> E.TransactionTemplate -> IO ()
insertTransactionTemplate file t = do
  -- print $ show cmd
  catchSql 
    (withDB file $ \conn -> execute conn cmd)
    (\e -> print $ show e)
  where cmd = "INSERT INTO TransactionTemplate VALUES(" ++ values ++ ");"
        values = formatValues [ "NULL" -- autoincremented primary key
                              , toSqlValue $ E.transactionTemplateName t
                              , toSqlValue $ acc2str $ accId $ E.transactionTemplatePayerAccountPK t
                              , toSqlValue $ bic2str $ bankBic $ E.transactionTemplatePayerAccountPK t
                              , toSqlValue $ acc2str $ accId $ E.transactionTemplateBnfcAccountPK t
                              , toSqlValue $ bic2str $ bankBic $ E.transactionTemplateBnfcAccountPK t
                              , toSqlValue $ E.transactionTemplateAmount t
                              , toSqlValue $ E.transactionTemplateReason t
                              , toSqlValue $ E.transactionTemplateIsUrgent t
                              ]

-- not tested !!!  
findTransactionTemplateById :: FilePath -> Int -> IO E.TransactionTemplate
findTransactionTemplateById file xid = sqlQueryGetFirst $
                                       sqlQuery (withDB file) fetchTransactionTemplate $
                                       "SELECT * FROM TransactionTemplate " ++
                                       "WHERE transaction_template_id = " ++ idValue ++ ";"
                                         where idValue = toSqlValue xid

-- not tested !!!
listTransactionTemplate :: FilePath -> IO [E.TransactionTemplate]
listTransactionTemplate file = sqlQuery (withDB file) fetchTransactionTemplate $
                               "SELECT * FROM TransactionTemplate;"

--------------------------------------------------------------------------------
-- End
--------------------------------------------------------------------------------
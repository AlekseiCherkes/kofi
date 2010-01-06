module Message
    where


import Types
import System.Time


--------------------------------------------------------------------------------
-- Messages
--------------------------------------------------------------------------------
data SenderId = ClientId { unp :: UNP }  -- Client UNP
              | BankId   { bic :: String }  -- BIC (ignored now)
              deriving (Read, Show)

data Message = Message { senderId :: SenderId
                       , digest :: Base64
                       , body :: EncryptedMessageBody }
             deriving (Read, Show)
                      
--------------------------------------------------------------------------------
-- Requests
--------------------------------------------------------------------------------

data TransactionPriority = Urgent | Normal
                           deriving (Read, Show)

isUrgent :: TransactionPriority -> Bool
isUrgent Normal = False
isUrgent Urgent = True

data CommitedTransaction = CommitedTransaction { reason :: String
                                               , creditAccount :: AccountPK
                                               , debitAccount :: AccountPK
                                               , amount :: Double
                                               , priority :: TransactionPriority }
                         deriving (Read, Show)

data Request = CommitTransaction CommitedTransaction
             | GetBalance AccountPK
             | GetStatement AccountPK CalendarTime CalendarTime -- без ошибок
             | GetLog AccountPK CalendarTime CalendarTime -- с ошибками
             deriving (Read, Show)
                      
--------------------------------------------------------------------------------
-- Responses
--------------------------------------------------------------------------------

data StatementRecord = StatementRecord { trnId :: Integer
                                       , commitDate :: CalendarTime
                                       , reciveDate :: CalendarTime
                                       , trnReason :: String
                                       , payerAcc :: AccountPK
                                       , bnfcAcc :: AccountPK
                                       , trnAmount :: Double 
                                       , trnPriority :: TransactionPriority }
                     deriving (Read, Show)

data LogRecordStatus = Bad String
                     | Good
                     deriving (Read, Show)
                        
data LogRecord = LogRecord { statement :: StatementRecord
                           , status :: LogRecordStatus }
               deriving (Read, Show)

data Response = Balance Double
              | Statement Double [StatementRecord]
              | Log [LogRecord]
              | Error String
              deriving (Read, Show)

--------------------------------------------------------------------------------
-- End
--------------------------------------------------------------------------------

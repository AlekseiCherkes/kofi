module Entity
    where 

--------------------------------------------------------------------------------

import System.Time

--------------------------------------------------------------------------------


data Account = Account { acc_id :: String
                       , bank_bic :: String
                       , owner_unp :: String
                       , ballance :: Double
                       , open_date :: CalendarTime
                       , close_date :: Maybe CalendarTime 
                       }               
               deriving (Read, Show)

data TransactionPriority = TransactionPriority Int
                           deriving (Read, Show)

data Transaction = Transaction { transactionId :: Integer
                               , commitDate :: CalendarTime
                               , reciveData :: CalendarTime
                               , transactionStatusId :: Integer
                             --, content :: String
                               , reason :: String
                               , creditAccountId :: String
                               , debitAccountId :: String
                               , amount :: Double
                               , priority :: TransactionPriority
                               }
                   deriving (Read, Show)

data Status = Status { statusId :: Integer
                     , message :: String 
                     }
              deriving (Read, Show)

--------------------------------------------------------------------------------



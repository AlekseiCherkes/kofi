module Entity
    where 

--------------------------------------------------------------------------------

import System.Time

--------------------------------------------------------------------------------

data Company = Company { unp :: String
                       , name :: String
                       , registryDate :: CalendarTime
                       , unregistryDate :: Maybe CalendarTime
                       , openKey :: String
                       }
               deriving (Read, Show)

data Account = Account { accountId :: String
                       , ownerId :: String
                       , ballance :: Double
                       , openData :: CalendarTime
                       , closeDate :: Maybe CalendarTime 
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



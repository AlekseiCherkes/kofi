module Entity
    where 

--------------------------------------------------------------------------------

import System.Time

--------------------------------------------------------------------------------

data Company = Company { unp :: Integer
                       , name :: String
--                       , registryDate :: CalendarTime
--                       , unregistryDate :: Maybe CalendarTime
                       , openKey :: Integer 
                       }
               deriving (Read, Show)

data Account = Account { accountId :: Integer
                       , ownerId :: Integer
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
                               , creditAccountId :: Integer
                               , debitAccountId :: Integer
                               , amount :: Double
                               , priority :: TransactionPriority
                               }
                   deriving (Read, Show)

data Status = Status { statusId :: Integer
                     , message :: String 
                     }
              deriving (Read, Show)

--------------------------------------------------------------------------------


                                           


            
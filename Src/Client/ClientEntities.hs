module ClientEntities where
import System.Time
import Types

data Company = Company{cmpName :: Name
                      ,cmpUnp  :: UNP}
               deriving (Read, Show)


data Bank = Bank{bnkName :: Name
                ,bnkBic  :: BIC}
            deriving (Read, Show)

data Account = Account{accPk     :: AccountPK
                      ,accCompany:: UNP}
               deriving (Read, Show)



data Profile = Profile{ profileUnp  :: UNP
                      , profileName :: Name
                      , profileDate :: CalendarTime
                      }

data Session = Session{ sessionProfile  :: Profile 
                      , sessionPath     :: FilePath
                      , sessionSendKey  :: RSAKey
                      , sessionRecvKey  :: RSAKey 
                      }

data Statement = Statement { statementId :: Int
                           , statementStartDate :: CalendarTime
                           , statementEndDate :: CalendarTime 
                           , statementAccountPK :: AccountPK
                           , statementText :: String
                           }
               deriving (Read, Show)

data TransactionTemplate = TransactionTemplate { transactionTemplateId :: Int
                                               , transactionTemplateName :: String
                                               , transactionTemplatePayerAccountPK :: AccountPK
                                               , transactionTemplateBnfcAccountPK :: AccountPK
                                               , transactionTemplateAmount :: Double
                                               , transactionTemplateReason :: String
                                               , transactionTemplateIsUrgent :: Bool 
                                               }
                             deriving (Read, Show)


module ClientEntities where
import Types
import Message (AccountPK)

data Company = Company{cmpName :: Name
                      ,cmpUnp  :: UNP}
               deriving (Read, Show)


data Bank = Bank{bnkName :: Name
                ,bnkBic  :: BIC}
            deriving (Read, Show)

data Account = Account{accPK     :: AccountPK
                      ,accBank   :: BIC
                      ,accCompany:: UNP}
               deriving (Read, Show)
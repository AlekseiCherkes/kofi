module ClientEntities where
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

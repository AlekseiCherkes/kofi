module DataModel where

import System.IO
import Validation



data Company = Company{cmpName : Name
                      ,cmpUnp  : UNP}


data Bank = Bank{bnkName : Name
                ,bnkBic  : BIC}

data Account = Account{accNumber : ACC
                      ,accBank   : BIC
                      ,accCompany: UNP}





findCompanyByName :: Name -> IO Company
findCompanyByName name = return $ Company name (str2unp "000000001")

findCompanyByUnp  :: UNP -> IO Company
findCompanyByUnp unp   = return $ Company "Company 1" unp

findCompaniesByBank :: BIC -> IO [Company]
findCompaniesByBank bic = return []




findBankByName :: Name -> IO Bank
findBankByName name = return $ Bank name (str2bic "001")

findBankByBic :: BIC -> IO Bank
findBankByBic bic = return $ Bank "Bank 1" bic

findBanksByCompany :: UNP -> IO [Bank]
findBanksByCompany unp = return []




findAccountsByCompany :: UNP -> IO [Account]
findAccountsByCompany unp = return []

findAccountsByBank :: BIC -> IO [Account]
findAccountsByBank bic = return []

findAccounts :: BIC -> ACC -> IO Account
findAccounts bic acc = return Account acc bic (str2unp "0000000001")




findCounterparties :: UNP -> IO [Company]
findCounterparties unp = return []






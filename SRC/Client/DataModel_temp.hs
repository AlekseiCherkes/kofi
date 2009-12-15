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
---
connect to client.db
select * 
from Company
where company_name = @Name
--

findCompanyByUnp  :: UNP -> IO Company
findCompanyByUnp unp   = return $ Company "Company 1" unp
---
connect to client.db
select * 
from Company
where company_unp = @UNP
--

findCompaniesByBank :: BIC -> IO [Company]
findCompaniesByBank bic = return []
---
connect to client.db
select Company.* 
from Company
inner join Account on Account.company_unp = Company.company_unp
where Account.bank_bic = @BIC
--




findBankByName :: Name -> IO Bank
findBankByName name = return $ Bank name (str2bic "001")
---
connect to manual.db
select * 
from Branch
where name = @Name
--

findBankByBic :: BIC -> IO Bank
findBankByBic bic = return $ Bank "Bank 1" bic
---
connect to manual.db
select * 
from Branch
where branch_bic = @BIC
--

findBanksByCompany :: UNP -> IO [Bank]
findBanksByCompany unp = return []
---
1) connect to client.db
@bank_bics = (select Account.bank_bic
	from Company
	inner join Account on Account.company_unp = Company.company_unp
	where Company.company_unp = @UNP
)
2) connect to manual.db
select *
from Branch
where branch_bic in @bank_bics
--



findAccountsByCompany :: UNP -> IO [Account]
findAccountsByCompany unp = return []
---
connect to client.db
select * 
from Account
where company_unp = @UNP
--

findAccountsByBank :: BIC -> IO [Account]
findAccountsByBank bic = return []
---
connect to client.db
select * 
from Account
where bank_bic = @BIC
--

findAccounts :: BIC -> ACC -> IO Account
findAccounts bic acc = return Account acc bic (str2unp "0000000001")
---
connect to client.db
select * 
from Account
where acc_id = @ACC
	and bank_bic = @BIC
--



findBeneficiaries :: UNP -> IO [Company]
findBeneficiaries unp = return []
---
connect to client.db
select * 
from Beneficiary
where beneficiary_unp = @UNP
--







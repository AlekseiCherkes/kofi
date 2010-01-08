module PrintTransaction 
       where

import DataModel
import Message

printTransaction :: FilePath -> CommitedTransaction -> IO String
printTransaction file ct = do
  pa <- findAccount (creditAccount ct)
  ba <- findAccount (debitAccount ct)
  
  -- bb <- findCompanyByUnp
  -- findBankByBic
  -- findCompanyByUnp
  let payerBankName = "Payer bank name"
  let payerBankBic = bic2str $ bankBic pa
  let payerAcc = acc2str $ accId pa
      
  let bnfcUnp = "123456789012"
  let bnfcName = "Bnfc Name"
  let bnfcBankName = "Bnfc Bank name"
  let bnfcBankBic = bic2str $ bankBic ba
  let bnfcAccId = acc2str $ accId ba
      
  let result = "|----------------------------------------" ++ "\n" ++
               "|Плательщик                 | " ++ "\n" ++
               "|    Банк плательнщика      | " ++ payerBankName ++ "\n" ++
               "|    BIC банка алательщика  | " ++ payerBankBic ++ "\n" ++
               "|    Номер счета плательщика| " ++ payerAcc ++ "\n" ++
               "|---------------------------|------------" ++ "\n" ++
               "|Бенефициар                 | " ++ "\n" ++
               "|    УНП бенефициара        | " ++ bnfcUnp ++ "\n" ++
               "|    Бенефициар             | " ++ bnfcName ++ "\n" ++
               "|    Банк бенефициара       | " ++ bnfcBankName ++ "\n" ++
               "|    BIC банка бенефициара  | " ++ bnfcBankBic ++ "\n" ++
               "|    Номер счета бенефициара| " ++ bnfcAccId ++ "\n" ++
               "|---------------------------|------------" ++ "\n" ++
               "|Поручение                  | " ++ "\n" ++
               "|   Сумма                   | " ++ (show $ amount ct) ++ "\n" ++
               "|   Срочность платежа       | " ++ (show $ priority ct) ++ "\n" ++
               "|   Назначение платежа      | " ++ (show $ reason ct) ++ "\n" ++
               "|                           | " ++ "\n" ++
               "|----------------------------------------" ++  "\n"
               
  return result
               
      
  
module PrintTransaction 
       where

import DataModel
import Message
import Types
import ClientEntities

import System.IO

printTransaction :: FilePath -> CommitedTransaction -> IO String
printTransaction file ct = do
  let papk = creditAccount ct
  let bapk = debitAccount ct
      
  pa <- findAccount file (bankBic papk) (accId papk)
  ba <- findAccount file (bankBic bapk) (accId bapk)
  
  pb <- findBankByBic (bankBic papk)
  bb <- findBankByBic (bankBic papk)
  
  bc <- findCompanyByUnp file (accCompany ba)
  
  let payerBankName = bnkName pb
  let payerBankBic = bic2str $ bankBic papk
  let payerAcc = acc2str $ accId papk
      
  let bnfcUnp = show $ cmpUnp bc
  let bnfcName = cmpName bc
  let bnfcBankName = bnkName bb
  let bnfcBankBic = bic2str $ bankBic bapk
  let bnfcAccId = acc2str $ accId bapk
      
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
               
  print result
               
  return result


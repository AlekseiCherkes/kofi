module Printers
       where

import DataModel
import Message
import Types
import qualified ClientEntities as CE

import System.IO
import System.Time
import System.Locale

printTransaction :: FilePath -> CommitedTransaction -> IO String
printTransaction file ct = do
  let papk = creditAccount ct
  let bapk = debitAccount ct
      
  pa <- findAccount file (bankBic papk) (accId papk)
  ba <- findAccount file (bankBic bapk) (accId bapk)
  
  pb <- findBankByBic (bankBic papk)
  bb <- findBankByBic (bankBic papk)
  
  bc <- findCompanyByUnp file (CE.accCompany ba)
  
  let payerBankName = CE.bnkName pb
  let payerBankBic = bic2str $ bankBic papk
  let payerAcc = acc2str $ accId papk
      
  let bnfcUnp = show $ CE.cmpUnp bc
  let bnfcName = CE.cmpName bc
  let bnfcBankName = CE.bnkName bb
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
               
  return result
  
printStatement :: Response -> AccountPK -> IO String
printStatement (Statement cb srs) apk = do
  let printRecord sr = 
        "| " ++ (show $ trnId sr)++ "\t" ++ 
        date ++ "\t" ++
        acc ++ "\t" ++
        (show $ trnAmount sr) ++ "\t" ++
        (show $ trnPriority sr) ++ "\t" ++
        (trnReason sr) ++ "\n"
        where date = (formatCalendarTime defaultTimeLocale "%Y-%m-%d %T" $ 
                      commitDate sr)
              sign = if (payerAcc sr) == apk
                     then "-"
                     else "+"
                          
              acc = if (payerAcc sr) == apk
                    then acc2str $ accId (payerAcc sr)
                    else acc2str $ accId (bnfcAcc sr)
  
  let result = 
        "|--------------------------------------------------\n" ++
        (foldl (++) "" $ map printRecord (srs)) ++
        "|--------------------------------------------------\n" ++
        " Closing ballance: " ++ (show cb) ++ "\n"
      
  return result


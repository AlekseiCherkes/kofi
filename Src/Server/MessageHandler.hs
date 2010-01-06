module MessageHandler
    where

import Types
import Crypto
import Loggers
import qualified Message as MSG
import DataModel hiding (infoM, errorM)
import Teller

import Data.Maybe
import Control.Exception
import Prelude hiding (catch)
import qualified System.Log.Logger as Logger

import Control.Monad.Error
import Control.Concurrent.Chan

--------------------------------------------------------------------------------
-- Logging utility functions
--------------------------------------------------------------------------------

infoM = Logger.infoM "root.client"
errorM = Logger.errorM "root.client"

--------------------------------------------------------------------------------
-- Main handler
--------------------------------------------------------------------------------

handleMessage :: (Chan Transaction) -> (Chan Transaction) -> String -> IO (Maybe String)
handleMessage urgents normals cnts = catch (perform cnts) handle
  where 
    handle e = (errorM $ "Error while handle user message: " 
               ++ (show (e :: SomeException)))
               >> return Nothing
    perform cnts = do
          -- 1) Чтение UNP из заголовка сообщения.
          let msg = (read cnts) :: MSG.Message
          let unp = MSG.unp $ MSG.senderId msg

          -- 2) Обращение в БД для поиска ключа ЭЦП данного UNP.          
          cmp <- (findCompanyByUNP unp) >>= \c ->
            if (isJust c)
              then (infoM $ "Found company: " ++ (show $ fromJust c))
                   >> return (fromJust c)
              else ioError 
                   (userError $ "Can't find company with unp = " 
                    ++ (unp2str unp) ++ " in server database.")
                   
          let recvKey = companyServerRecvKey $ cmp
          let sendKey = companyServerSendKey $ cmp

          infoM $ "Keys for use: " ++
            "recvKey: " ++ (show recvKey) ++ ", " ++
            "sendKey: " ++ (show sendKey)

          -- 3) Проверка ЭЦП.
          if (verifyMessage recvKey msg)
            then infoM $ "Digest verification complete successfully."
            else ioError (userError "Digest mismatch.")

          -- 4) Расшифровка сообщения.
          let dmb = decodeMessageBody recvKey (MSG.body msg)
          infoM $ "Decoded message body: " ++ dmb
    
          let r = (read dmb) :: MSG.Request
              
          -- Формирование ответа
          response <- case r of
            MSG.CommitTransaction ct -> return $ MSG.Error "CommitTransaction not implemented."
            MSG.GetBalance apk -> runHandler $ getBallance cmp apk
            MSG.GetStatement apk ct1 ct2 -> return $ MSG.Error "GetStatement not implemented."
            MSG.GetLog apk ct1 ct2 -> return $ MSG.Error "GetLog not implemented."
            
          -- кодирование и возврат ответа
            
          infoM $ "Response: " ++ (show response)
            
          return $ Just $ show $ 
            createMessage sendKey (MSG.BankId $ "0") $
            encodeMessageBody sendKey (show response)

--------------------------------------------------------------------------------
-- Messages
--------------------------------------------------------------------------------

type MessageMonad = ErrorT String IO MSG.Response

runHandler :: MessageMonad -> IO MSG.Response
runHandler handler = do
  res <- runErrorT $ handler
  return $ 
    case res of
      (Right r) -> r
      (Left e) -> MSG.Error e
    
--------------------------------------------------------------------------------
-- Checkers
--------------------------------------------------------------------------------

checkAcc acc = do
  if (isJust acc)
    then do
    liftIO $ infoM $ "Found account: " ++ (show $ fromJust acc)
    return $ fromJust acc
    else throwError $ "Can't find account by apk in server database."

checkDate field = do
  if (isNothing $ field)
    then liftIO $ infoM $ "Entity is opened: " ++ (show field)
    else throwError $ "Entity closed: " ++ (show field)

checkBank bnk = do
  if (isJust bnk)
    then do
    liftIO $ infoM $ "Found bank: " ++ (show $ fromJust bnk)
    return $ fromJust bnk   
    else throwError $ 
         "Can't find bank by UNP = " ++ 
         -- (show $ bankBic $ acc_pk acc) ++ 
         "in banks manual database."
  
checkAccountOwner acc cmp = do
  if ((accountOwnerUnp acc) == (companyUnp cmp)) 
    then liftIO $ infoM "Account belongs to author of the reqest."
    else throwError "Author of the request hasn't have responsed account."

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

getBallance :: Company -> AccountPK -> MessageMonad
getBallance cmp apk = do
  acc <- (liftIO $ findAccountByPK apk) >>= checkAcc
  bnk <- (liftIO $ findBankByBIC (bankBic $ accountPK $ acc)) >>= checkBank
  checkAccountOwner acc cmp
  checkDate $ companyUnregistryDate cmp
  checkDate $ accountCloseDate acc
  return $ MSG.Balance (accountBallance acc)

--------------------------------------------------------------------------------
-- End of file
--------------------------------------------------------------------------------

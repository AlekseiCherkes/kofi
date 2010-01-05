module MessageHandler
    where

import Types
import Crypto
import Loggers
import qualified Message as MSG
import qualified DataModel as DM

import Data.Maybe
import System.IO
import System.IO.Error hiding (catch)
import Control.Exception
import Prelude hiding (catch)
import qualified System.Log.Logger as Logger

import Control.Monad.Error

--------------------------------------------------------------------------------
-- Logging utility functions
--------------------------------------------------------------------------------

infoM = Logger.infoM "root.client"
errorM = Logger.errorM "root.client"

--------------------------------------------------------------------------------
-- Main handler
--------------------------------------------------------------------------------

handleMessage :: String -> IO (Maybe String)
handleMessage cnts = catch (perform cnts) handle
  where 
    handle e = (errorM $ "Error while handle user message: " 
               ++ (show (e :: SomeException)))
               >> return Nothing
    perform cnts = do
          -- 1) Чтение UNP из заголовка сообщения.
          let msg = (read cnts) :: MSG.Message
          let unp = MSG.unp $ MSG.senderId msg

          -- 2) Обращение в БД для поиска ключа ЭЦП данного UNP.          
          cmp <- (DM.findCompanyByUNP unp) >>= \c ->
            if (isJust c)
              then (infoM $ "Found company: " ++ (show $ fromJust c))
                   >> return (fromJust c)
              else ioError 
                   (userError $ "Can't find company with unp = " 
                    ++ unp ++ " in server database.")
                   
          let recvKey = DM.serverRecvKey $ cmp
          let sendKey = DM.serverSendKey $ cmp

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
            MSG.GetBalance apk -> getBalance cmp apk
            MSG.GetStatement apk ct1 ct2 -> return $ MSG.Error "GetStatement not implemented."
            MSG.GetLog apk ct1 ct2 -> return $ MSG.Error "GetLog not implemented."
            
          -- кодирование и возврат ответа
            
          infoM $ "Response: " ++ (show response)
            
          return $ Just $ show $ 
            createMessage sendKey (MSG.BankId "0") $
            encodeMessageBody sendKey (show response)

--------------------------------------------------------------------------------
-- Messages
--------------------------------------------------------------------------------

type MessageMonad = ErrorT String IO MSG.Response

getBalance :: DM.Company -> AccountPK -> IO MSG.Response
getBalance cmp apk = do
  res <- runErrorT $ perform cmp apk
  return $ 
    case res of
    (Right r) -> r
    (Left error_msg) -> MSG.Error error_msg
    
perform :: DM.Company -> AccountPK -> MessageMonad
perform cmp apk = do
  acc <- liftIO $ DM.findAccountByPK apk
  if (isJust acc)
    then liftIO $ infoM $ "Found account: " ++ (show $ fromJust acc)
    else throwError $ 
         "Can't find account by apk = " ++ 
         (show apk) ++ " in server database."
         
  bnk <- liftIO $ DM.findBankByBIC (DM.bank_bic $ fromJust acc)
  if (isJust bnk)
    then liftIO $ infoM $ "Found accounts bank: " ++ (show $ fromJust bnk)
    else throwError $ 
         "Can't find account's bank by UNP = " ++ 
         (show $ DM.bank_bic $ fromJust acc) ++ 
         "in banks manual database."

  if ((DM.owner_unp $ fromJust acc) == (DM.unp cmp)) 
    then liftIO $ infoM "Account belongs to author of the reqest."
    else throwError "Author of the request hasn't have responsed account."

  return $ MSG.Balance (DM.ballance $ fromJust acc)

--------------------------------------------------------------------------------
-- End of file
--------------------------------------------------------------------------------

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
                    ++ (unp2str unp) ++ " in server database.")
                   
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
            MSG.CommitTransaction ct -> return $ Just "CommitTransaction not implemented."
            MSG.GetBalance apk -> getBalance cmp apk
            MSG.GetStatement apk ct1 ct2 -> return $ Just "GetStatement not implemented."
            MSG.GetLog apk ct1 ct2 -> return $ Just "GetLog not implemented."
            
          return response

--------------------------------------------------------------------------------
-- Messages
--------------------------------------------------------------------------------

getBalance cmp apk = do
  akk <- (DM.findAccountByPK apk) >>= \a ->
    if (isJust a)
    then (infoM $ "Found account: " ++ (show $ fromJust a))
         >> return (fromJust a)
    else ioError 
         (userError $ "Can't find account by apk = " 
          ++ (show apk) ++ " in server database.")

  return $ Just $ show (DM.ballance akk)

--------------------------------------------------------------------------------
-- End of file
--------------------------------------------------------------------------------

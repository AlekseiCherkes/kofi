module MessageHandler
    where

import Types
import Crypto
import Loggers
import Message
import DataModel hiding (infoM, errorM)
import qualified Teller as TLR

import Data.Maybe
import Control.Exception
import Prelude hiding (catch)
import qualified System.Log.Logger as Logger

import System.Time

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

handleMessage :: (Chan TLR.Transaction) -> (Chan TLR.Transaction) -> String -> IO (Maybe String)
handleMessage urgents normals cnts = catch (perform cnts) handle
  where 
    handle e = (errorM $ "Error while handle user message: " 
               ++ (show (e :: SomeException)))
               >> return Nothing
    perform cnts = do
      
          timestamp <- getClockTime >>= toCalendarTime
      
          -- 1) Чтение UNP из заголовка сообщения.
          let msg = (read cnts) :: Message
          let unp = Message.unp $ senderId msg

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
          let dmb = decodeMessageBody recvKey (body msg)
          infoM $ "Decoded message body: " ++ dmb
    
          let r = (read dmb) :: Request
              
          -- Формирование ответа
          response <- case r of
            CommitTransaction ct -> pushTransaction urgents normals timestamp cmp ct
            GetBalance apk -> runHandler $ getBallance cmp apk
            GetStatement apk ct1 ct2 -> runHandler $ getStatement cmp apk ct1 ct2
            GetLog apk ct1 ct2 -> runHandler $ getLog cmp apk ct1 ct2
            
          -- кодирование и возврат ответа
          
          case response of
            Nothing -> return Nothing
            Just a -> do 
              infoM $ "Response: " ++ (show response)
              return $ Just $ show $
                createMessage sendKey (BankId $ "0") $
                encodeMessageBody sendKey (show response)

--------------------------------------------------------------------------------
-- Messages
--------------------------------------------------------------------------------

type MessageMonad = ErrorT String IO Response

runHandler :: MessageMonad -> IO (Maybe Response)
runHandler handler = do
  res <- runErrorT handler
  return $ 
    case res of
      Right r -> Just r
      Left e -> Just $ Error e
    
--------------------------------------------------------------------------------
-- Checkers
--------------------------------------------------------------------------------

retriveAcc apk = do
  acc <- liftIO $ findAccountByPK apk
  if (isJust acc)
    then do
    liftIO $ infoM $ "Account retrived: " ++ (show $ fromJust acc)
    return $ fromJust acc
    else throwError $ "Can't retrive account by apk = " ++ (show apk)

retriveBank bic = do
  bnk <- liftIO $ findBankByBIC bic
  if (isJust bnk)
    then do
    liftIO $ infoM $ "Bank retrived: " ++ (show $ fromJust bnk)
    return $ fromJust bnk
    else throwError $ "Can't retrive bank by BIC = " ++ (show bic)

checkDate field = do
  if (isNothing $ field)
    then liftIO $ infoM $ "Entity is opened: " ++ (show field)
    else throwError $ "Entity closed."
  
checkAccountOwner acc cmp = do
  if ((accountOwnerUnp acc) == (companyUnp cmp)) 
    then liftIO $ infoM "Account belongs to author of the reqest."
    else throwError "Author of the request hasn't have responsed account."

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

getBallance :: Company -> AccountPK -> MessageMonad
getBallance cmp apk = do
  acc <- retriveAcc apk
  bnk <- retriveBank $ bankBic $ accountPK acc
  checkAccountOwner acc cmp
  checkDate $ companyUnregistryDate cmp
  checkDate $ accountCloseDate acc
  return $ Balance (accountBallance acc)

pushTransaction :: (Chan TLR.Transaction) -> 
                  (Chan TLR.Transaction) -> 
                  CalendarTime -> Company -> CommitedTransaction -> IO (Maybe Response)                  
pushTransaction urgents normals timestamp cmp ct = do
  let trn = TLR.Transaction (companyUnp cmp) 
            (creditAccount ct) (debitAccount ct) 
            (amount ct) timestamp (show ct) (reason ct) (priority ct)
      
  case (priority ct) of
    Urgent -> liftIO $ do
      infoM "Insert commited transaction in urgents channel"
      writeChan urgents trn
    Normal -> liftIO $ do 
      infoM "Insert commited transaction in normals channel"
      writeChan normals trn

  return Nothing
  
getStatement :: Company -> AccountPK -> CalendarTime -> CalendarTime -> MessageMonad
getStatement cmp apk from to = do
  acc <- retriveAcc apk
  bnk <- retriveBank $ bankBic $ accountPK acc
  checkAccountOwner acc cmp
  checkDate $ companyUnregistryDate cmp
  checkDate $ accountCloseDate acc

  if to < from
    then do
    throwError "End date is less then start date."
    else do
    liftIO $ infoM "End date is great then start date."
    
  ts <- liftIO $ findTransactionsForStatement apk from to
  
  case ts of
    Nothing -> throwError "Can't retrive transaction list."
    Just jts -> return $ Statement 0.0 $ map trn2record jts
    
    where trn2record t = StatementRecord 
                         (transactionId t)
                         (transactionCommitDate t)
                         (transactionReciveDate t)
                         (transactionReason t)
                         (transactionPayerAccountPK t)
                         (transactionBnfcAccountPK t)
                         (transactionAmount t)
                         (transactionPriority t)
  
getLog :: Company -> AccountPK -> CalendarTime -> CalendarTime -> MessageMonad
getLog cmp apk from to = do
  acc <- retriveAcc apk
  bnk <- retriveBank $ bankBic $ accountPK acc
  checkAccountOwner acc cmp
  checkDate $ companyUnregistryDate cmp
  checkDate $ accountCloseDate acc

  if to < from
    then do
    throwError "End date is less then start date."
    else do
    liftIO $ infoM "End date is great then start date."
    
  ts <- liftIO $ findTransactionsForLog apk from to
  
  case ts of
    Nothing -> throwError "Can't retrive transaction list."
    Just jts -> do
      lrs <- liftIO $ mapM trn2lr jts
      return $ Log lrs
    
  where trn2lr t = if errno == 0
                   then return $ LogRecord sr Good
                   else do
                     st <- errst
                     return $ LogRecord sr $ Bad st
                     where errno = transactionStatusId t
                           errst = do
                             res <- findTransactionStatusMessageById errno
                             return $ fromJust res
                           sr =  StatementRecord 
                                   (transactionId t)
                                   (transactionCommitDate t)
                                   (transactionReciveDate t)
                                   (transactionReason t)
                                   (transactionPayerAccountPK t)
                                   (transactionBnfcAccountPK t)
                                   (transactionAmount t)
                                   (transactionPriority t)


--------------------------------------------------------------------------------
-- End of file
--------------------------------------------------------------------------------

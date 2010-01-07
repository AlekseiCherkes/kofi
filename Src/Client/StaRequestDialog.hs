
module StaRequestDialog where

-- standard imports
import System.IO
import System.Time 
import Data.IORef
import Control.Monad (liftM2)

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- Common imports
import Types
import Message


-- Client imports
import GtkCommon
import ClientEntities
import AccountChooser (showAccountChooser)
import WaitDialog     (showWaitDialog)


data StaRequestDialog = StaRequestDialog { dialog_wnd    :: Dialog
                                         , changeAcc_btn :: Button
                                         , commit_btn    :: Button
                                         , cancel_btn    :: Button
                                         , bank_lbl      :: Label
                                         , bic_lbl       :: Label
                                         , acc_lbl       :: Label
                                         , from_clnd     :: Calendar
                                         , till_clnd     :: Calendar
                                         , account       :: IORef (Maybe AccountPK   )
                                         , start_time    :: IORef (Maybe CalendarTime)
                                         , end_time      :: IORef (Maybe CalendarTime)
                                         }
                                         

loadStaRequestDialog :: FilePath -> IO StaRequestDialog
loadStaRequestDialog gladePath = do
    Just glade <- xmlNew gladePath
  
    dialog_wnd   <- xmlGetWidget glade castToDialog        "dialog_wnd"
    
    [ changeAcc_btn ,  commit_btn ,  cancel_btn ] <- mapM (xmlGetWidget glade castToButton) [
     "changeAcc_btn", "commit_btn", "cancel_btn"]
     
    [ bank_lbl ,  bic_lbl ,  acc_lbl ] <- mapM (xmlGetWidget glade castToLabel) [
     "bank_lbl", "bic_lbl", "acc_lbl"] 
     
    [ from_clnd ,  till_clnd ]  <- mapM (xmlGetWidget glade castToCalendar) [
     "from_clnd", "till_clnd"] 
     
    account    <- newIORef Nothing
    start_time <- newIORef Nothing
    end_time   <- newIORef Nothing
     
    return $ StaRequestDialog 
                dialog_wnd
                changeAcc_btn
                commit_btn
                cancel_btn
                bank_lbl
                bic_lbl
                acc_lbl
                from_clnd
                till_clnd
                account
                start_time
                end_time
                
initStaRequestDialog :: StaRequestDialog -> Session -> (IO (Maybe (AccountPK, Name))) -> IO ()
initStaRequestDialog gui session chooseAcc = do
    let path = sessionPath session
    
    onClicked (changeAcc_btn gui) $ do
        chosenAcc <- chooseAcc
        case chosenAcc of
            Nothing         -> return ()
            Just (accpk, _) -> do
                writeIORef (account gui) (Just accpk)
                renderAccountInfo chosenAcc (bank_lbl gui) (bic_lbl gui) (acc_lbl gui)
        validateStaRequestDialog gui
        return ()
        
    let endc = till_clnd gui
    let strc = from_clnd gui    
    let onEndDate   = onDateChanged gui endc (end_time   gui) True
    let onStartDate = onDateChanged gui strc (start_time gui) False
    
    onDaySelected  strc onStartDate
    onMonthChanged strc onStartDate
    onNextMonth    strc onStartDate
    onPrevMonth    strc onStartDate
    onNextYear     strc onStartDate
    onPrevYear     strc onStartDate
    
    onDaySelected  endc onEndDate
    onMonthChanged endc onEndDate
    onNextMonth    endc onEndDate
    onPrevMonth    endc onEndDate
    onNextYear     endc onEndDate
    onPrevYear     endc onEndDate
    
    
    onClicked (commit_btn gui) $ do
        isValid <- validateStaRequestDialog gui
        if isValid 
            then do
                commitStaRequest gui session
                dialogResponse (dialog_wnd  gui) ResponseOk
            else showWarningMessage gui

    onClicked (cancel_btn gui) (dialogResponse (dialog_wnd  gui) ResponseCancel)
    return ()
        
onDateChanged :: StaRequestDialog -> Calendar -> IORef (Maybe CalendarTime) -> Bool -> IO ()
onDateChanged gui clndr timeRef roundUp = do
    ctime <- date2time  clndr roundUp 
    writeIORef timeRef (Just ctime) 
    validateStaRequestDialog gui
    return ()
    
    
validateStaRequestDialog :: StaRequestDialog -> IO Bool
validateStaRequestDialog gui = do
    let isSet = \getter -> isRefSet $ getter gui
    let andM  = liftM2 (&&)
    cond <-    isSet account 
        `andM` isSet start_time
        `andM` isSet end_time
        
    if cond 
        then do
            Just stt <- (readIORef . start_time ) gui
            Just ent <- (readIORef . end_time   ) gui
            setButtonSensitive (commit_btn gui  ) (stt < ent)
            return (stt < ent)
        else do
            setButtonSensitive (commit_btn gui  ) False
            return False
            
showWarningMessage :: StaRequestDialog -> IO ()
showWarningMessage gui = do
    msg_dialog <- messageDialogNew Nothing [DialogModal] MessageWarning ButtonsClose "Форма заполнена неверно."
    windowSetTransientFor msg_dialog (dialog_wnd gui)
    dialogRun msg_dialog
    widgetDestroy msg_dialog
    

getStaRequestData :: StaRequestDialog -> IO Request
getStaRequestData gui = do
    Just stt <- (readIORef . start_time ) gui
    Just ent <- (readIORef . end_time   ) gui
    Just acc <- (readIORef . account    ) gui
    
    return $ GetStatement acc stt ent
    
    
    
commitStaRequest :: StaRequestDialog -> Session -> IO ()
commitStaRequest gui session = do
    requst <- getStaRequestData gui
    mservResp <- showWaitDialog (dialog_wnd gui) session requst
    dialog <- case mservResp of
        Nothing               -> messageDialogNew Nothing [DialogModal] MessageInfo  ButtonsOk ("Запрос был отменен.")
        Just (Statement am xs)-> messageDialogNew Nothing [DialogModal] MessageInfo  ButtonsOk ("Запрос отправлен.")
        Just (Error   msg    )-> messageDialogNew Nothing [DialogModal] MessageError ButtonsClose ("Ошибка: " ++ msg ++ ".")
        otherwise             -> do
            msg_dialog <- messageDialogNew Nothing [DialogModal] MessageError ButtonsClose ("Сервер ответил неверно: ")
            messageDialogSetSecondaryText msg_dialog (show mservResp)
            return msg_dialog
    
    windowSetTransientFor dialog (dialog_wnd gui)
    dialogRun dialog
    widgetDestroy dialog
    

showStaRequestDialog :: Session -> IO ()
showStaRequestDialog session = do
    gui <- loadStaRequestDialog "Resources/staRequest_dialog.glade"
    let unp       = profileUnp $ sessionProfile session
    let chooseAcc = showAccountChooser (dialog_wnd gui) session unp
    
    initStaRequestDialog gui session chooseAcc
    validateStaRequestDialog gui
    onResponse (dialog_wnd gui) (\_ -> widgetDestroy $ dialog_wnd gui)
    widgetShowAll (dialog_wnd gui)


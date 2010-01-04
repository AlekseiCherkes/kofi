module MainWindow where

import Data.IORef


-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- Common imports



-- Client imports
import ClientEntities
import TransactionDialog (showTransactionDialog)
import BalanceDialog     (showBalanceDialog)
import StaRequestDialog  (showStaRequestDialog)
import GtkCommon


actionEntries = 
 [ActionEntry "SwitchUser_a"  "Переключить пользователя" (Just stockDialogAuthentication) Nothing (Just "Меняет пользователя."              ) (putStrLn "SwitchUser_a")--onSwitchUser 
 ,ActionEntry "NewPay_a"      "Создать"                  (Just stockAdd                 ) Nothing (Just "Создает новое платежное поручение" ) (putStrLn "NewPay_a")--onNewPay 
 ,ActionEntry "ViewPays_a"    "Просмотреть платежи"      (Just stockIndex               ) Nothing (Just "Показывает все платежные поручения") (putStrLn "ViewPays_a")--onViewPay 
 ,ActionEntry "StaReq_a"      "Запрсить выписку"         (Just stockDnd                 ) Nothing (Just "Запрашивает выписку со счета."     ) (putStrLn "StaReq_a")--onStaReq 
 ,ActionEntry "LogReq_a"      "Запрсить лог"             (Just stockUndelete            ) Nothing (Just "Запрашивает лог."                  ) (putStrLn "LogReq_a")--onLogReq 
 ,ActionEntry "ViewSta_a"     "Просмотреть выписки"      (Just stockDndMultiple         ) Nothing (Just "Показывает все выписки."           ) (putStrLn "ViewSta_a")--onViewSta 
 ,ActionEntry "ViewBalance_a" "Запрсить баланс"          (Just stockZoom100             ) Nothing (Just "Запрашивает баланс счета."         ) (putStrLn "ViewBalance_a")--onBalanceReq 
 ,ActionEntry "Exit_a"        "Выход"                    (Just stockQuit                ) Nothing (Just "Завершает программу."              ) (putStrLn "Exit_a") --mainQuit
 ,ActionEntry "About_a"       "О программе"              (Just stockAbout               ) Nothing (Just "About."                            ) (putStrLn "About_a")--onAbout
 ]


initMainActionGroup :: IO ActionGroup
initMainActionGroup = do
    usrAct <- actionNew "UsrAction"  "Пользователь" Nothing Nothing
    payAct <- actionNew "PayAction"  "Поручения"    Nothing Nothing
    staAct <- actionNew "StaAction"  "Выписки"      Nothing Nothing
    accAct <- actionNew "AccAction"  "Счет"         Nothing Nothing
    hlpAct <- actionNew "HlpAction"  "Справка"      Nothing Nothing

    mainGroup <- actionGroupNew "main"
    mapM_ (actionGroupAddAction mainGroup) [usrAct, payAct, staAct, accAct, hlpAct]
    actionGroupAddActions mainGroup actionEntries
    return mainGroup
  

loadMenuBar :: FilePath -> ActionGroup -> IO Widget
loadMenuBar xmlFile actions = do
    ui <- uiManagerNew
    uiManagerAddUiFromFile ui xmlFile
    uiManagerInsertActionGroup ui actions 0
    (Just menuBar) <- uiManagerGetWidget ui "/ui/menubar"
    return menuBar



data MainWindow = MainWindow {dialog_wnd :: Dialog
                             ,actions    :: ActionGroup
                             ,date_lbl   :: Label
                             ,name_lbl   :: Label
                             ,unp_lbl    :: Label
                             ,session    :: IORef (Maybe Session)
                             }  
   

loadMainWindow :: IO MainWindow
loadMainWindow = do
  actions <- initMainActionGroup
  putStrLn "Actions initialized"
  menuBar <- loadMenuBar "Resources/ActionMenu.xml" actions
  putStrLn "Menu loaded"
  Just glade <- xmlNew "Resources/mainWindow_1.glade"
  putStrLn "main glade loaded"
  dialog_wnd <- xmlGetWidget glade castToDialog "dialog_wnd"
  vBox       <- xmlGetWidget glade castToVBox   "dialog-vbox1"

  boxPackStart vBox menuBar PackNatural 0
  boxReorderChild vBox menuBar 0 
  
  --dialogSetDefaultResponse dialog_wnd ResponseOk
  
  [ date_lbl ,  name_lbl ,  unp_lbl ] <- mapM (xmlGetWidget glade castToLabel) [
   "date_lbl", "name_lbl", "unp_lbl"]
   
  session <- (newIORef Nothing) 
   
  return $ MainWindow dialog_wnd actions date_lbl name_lbl unp_lbl session
  
  
bindActions :: MainWindow -> Session -> IO ()
bindActions gui session = do
    let acts = actions gui
    
    (Just usrAction) <- actionGroupGetAction acts "SwitchUser_a"
    onActionActivate usrAction (dialogResponse (dialog_wnd  gui) ResponseOk)
    
    (Just exiAction) <- actionGroupGetAction acts "Exit_a"
    onActionActivate exiAction (dialogResponse (dialog_wnd  gui) ResponseClose)
    
    (Just payAction) <- actionGroupGetAction acts "NewPay_a"
    onActionActivate payAction (showTransactionDialog session)
    
    (Just accAction) <- actionGroupGetAction acts "ViewBalance_a"
    onActionActivate accAction (showBalanceDialog session)
    
    (Just staAction) <- actionGroupGetAction acts "StaReq_a"
    onActionActivate staAction (showStaRequestDialog session)
    return ()
 

setCompanyData :: MainWindow -> Session -> IO ()
setCompanyData gui session = do
    let profile = sessionProfile session
    renderProfileInfo profile (name_lbl gui) (unp_lbl gui) (date_lbl gui)
    
 
showMainWindow :: Session -> IO ResponseId
showMainWindow session = do
    gui <- loadMainWindow
    setCompanyData gui session
    bindActions    gui session
    responce <- dialogRun (dialog_wnd  gui)
    widgetDestroy (dialog_wnd gui)
    return responce




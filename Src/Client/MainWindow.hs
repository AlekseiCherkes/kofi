module MainWindow where


import System.Time



-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- Common imports
import Types


-- Client imports
import ClientEntities
import TransactionDialog (showTransactionDialog)
import BalanceDialog     (showBalanceDialog)
import StaRequestDialog  (showStaRequestDialog)
import ProfileChooser    (showProfileChooser)


actionEntries = 
 [ActionEntry "SwitchUser_a"  "Переключить пользователя" (Just stockDialogAuthentication) Nothing (Just "Меняет пользователя."              ) (putStrLn "SwitchUser_a")--onSwitchUser 
 ,ActionEntry "NewPay_a"      "Создать"                  (Just stockAdd                 ) Nothing (Just "Создает новое платежное поручение" ) (putStrLn "NewPay_a")--onNewPay 
 ,ActionEntry "ViewPays_a"    "Просмотреть платежи"      (Just stockIndex               ) Nothing (Just "Показывает все платежные поручения") (putStrLn "ViewPays_a")--onViewPay 
 ,ActionEntry "StaReq_a"      "Запрсить выписку"         (Just stockDnd                 ) Nothing (Just "Запрашивает выписку со счета."     ) (putStrLn "StaReq_a")--onStaReq 
 ,ActionEntry "LogReq_a"      "Запрсить лог"             (Just stockUndelete            ) Nothing (Just "Запрашивает лог."                  ) (putStrLn "LogReq_a")--onLogReq 
 ,ActionEntry "ViewSta_a"     "Просмотреть выписки"      (Just stockDndMultiple         ) Nothing (Just "Показывает все выписки."           ) (putStrLn "ViewSta_a")--onViewSta 
 ,ActionEntry "ViewBalance_a" "Запрсить баланс"          (Just stockZoom100             ) Nothing (Just "Запрашивает баланс счета."         ) (putStrLn "ViewBalance_a")--onBalanceReq 
 ,ActionEntry "Exit_a"        "Выход"                    (Just stockQuit                ) Nothing (Just "Завершает программу."              ) mainQuit
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



data MainWindow = MainWindow {window         :: Window
                             ,actions        :: ActionGroup
                             ,userName_lbl   :: Label
                             ,companyName_lbl:: Label
                             ,companyUnp_ldl :: Label
                             }  
   

loadMainWindow :: IO MainWindow
loadMainWindow = do
  actions <- initMainActionGroup
  putStrLn "Actions initialized"
  menuBar <- loadMenuBar "Resources/ActionMenu.xml" actions
  putStrLn "Menu loaded"
  Just glade <- xmlNew "Resources/mainWindow.glade"
  putStrLn "main glade loaded"
  window     <- xmlGetWidget glade castToWindow "mainWindow"
  vBox       <- xmlGetWidget glade castToVBox   "vbox1"

  boxPackStart vBox menuBar PackNatural 0
  boxReorderChild vBox menuBar 0 
  
  [user_l, comp_l, unp_l] <- mapM (xmlGetWidget glade castToLabel) ["user_lbl", "company_lbl", "cmpUnp_lbl"]
  return $ MainWindow window actions user_l comp_l unp_l
  
  
bindActions :: ActionGroup -> IO ()
bindActions actions = do
    clock <- getClockTime
    time  <- toCalendarTime clock 
    let session = Session  (Profile (str2unp "987654321123") "Some company." time) "FilePath"
    
    (Just usrAction) <- actionGroupGetAction actions "SwitchUser_a"
    onActionActivate usrAction (showProfileChooser >>= (\_->return ()))
    
    (Just payAction) <- actionGroupGetAction actions "NewPay_a"
    onActionActivate payAction (showTransactionDialog session)
    
    (Just accAction) <- actionGroupGetAction actions "ViewBalance_a"
    onActionActivate accAction (showBalanceDialog session)
    
    (Just staAction) <- actionGroupGetAction actions "StaReq_a"
    onActionActivate staAction (showStaRequestDialog session)
    return ()
 
 
showMainWindow :: IO ()
showMainWindow = do
    gui <- loadMainWindow
    bindActions   (actions gui)
    onDestroy     (window  gui) mainQuit
    widgetShowAll (window  gui)




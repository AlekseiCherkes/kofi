module MainWindow where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade


data GuiActions = GuiActions {switchUserAction     :: Action
                             ,newPaymentAction     :: Action
                             ,viewPaymentsAction   :: Action
                             ,staRequestAction     :: Action
                             ,logRequestAction     :: Action
                             ,viewStaAction        :: Action
                             ,balanceRequestAction :: Action
                             }
                             
data MainWindowControms = MainWindowControms {userName_lbl   :: Label
                                             ,companyName_lbl:: Label
                                             ,companyUnp_ldl :: Label
                                             }
                                            
data MainWindow = MainWindow {window  :: Window
                             ,actions :: GuiActions
                             ,controls:: MainWindowControms
                             }


loadMainWindow :: FilePath -> IO MainWindow
loadMainWindow gladePath = do
  Just glade <- xmlNew gladePath
  window <- xmlGetWidget glade castToWindow "mainWindow"
  [switch_act, newPay_act, viewPay_act ,staReq_act, logReq_act, viewSta_act ,balanc_act] <- mapM (widgetGetAction >>= xmlGetWidget glade castToAction) [ "switchUser_manui", "new_payment", "view_all_payments", "request_sta" , "request_log", "view_all_sta", "view_balance"] 
  [user_l, comp_l, unp_l] <- mapM (xmlGetWidget glade castToLabel) ["user_lbl", "company_lbl", "cmpUnp_lbl"]
  return (MainWindow window (GuiActions switch_act newPay_act viewPay_act logReq_act staReq_act viewSta_act balanc_act) (MainWindowControms user_l comp_l unp_l))
 


bindMainWindowSignals :: MainWindow -> IO ()
bindMainWindowSignals gui = do
    onDestroy (window gui) mainQuit
    
    let acts = (actions gui)
    
    onActionActivate (switchUserAction     acts) (putStrLn "Switch user")
    onActionActivate (newPaymentAction     acts) (putStrLn "New Payment")
    onActionActivate (viewPaymentsAction   acts) (putStrLn "View Payments user")
    onActionActivate (staRequestAction     acts) (putStrLn "Request Statement")
    onActionActivate (logRequestAction     acts) (putStrLn "Request logs")
    onActionActivate (viewStaAction        acts) (putStrLn "View Statements")
    onActionActivate (balanceRequestAction acts) (putStrLn "Request Balance")



module FxViewer where

import Data.List ( isPrefixOf, isSuffixOf)
import Data.Char ( toLower )

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- Common imports
import Message

-- Client imports
import ClientEntities
import GtkCommon
import WaitDialog (showWaitDialog)



data FxViewDialog = FxViewDialog { dialog_wnd :: Dialog
                                 , close_btn  :: Button
                                 , fx_tv      :: TreeView
                                 }
                                                 
loadFxViewer :: FilePath -> IO FxViewDialog
loadFxViewer gladePath = do
    Just glade <- xmlNew gladePath
  
    dialog_wnd <- xmlGetWidget glade castToDialog   "dialog_wnd"
    fx_tv      <- xmlGetWidget glade castToTreeView "fx_tv"
    close_btn  <- xmlGetWidget glade castToButton   "close_btn"
     
    
          
    return $ FxViewDialog dialog_wnd close_btn fx_tv



initFxViewer :: FxViewDialog -> [CurrencyRate] ->  IO ()
initFxViewer gui rates = do
    model <- listStoreNew rates
    
    initTreeViewColumns (fx_tv gui) model [
        ("Название курса" , \r -> (primaryName r) ++ "/" ++ (secondaryName r)   ),
        ("Значение курса" , \r -> show (rate r)                                 )]
    
    bindTreeViewHandlers 
        rateDoesMatch
        (\_ -> return ()) 
        (fx_tv gui) 
        model


rateDoesMatch :: MatchFunc CurrencyRate
rateDoesMatch str r  = map toLower str `isPrefixOf` map toLower ((primaryName r) ++ "/" ++ (secondaryName r))
    
    
showFxViewerDialog :: [CurrencyRate] -> IO ()
showFxViewerDialog rates = do
    gui <- loadFxViewer "Resources/fx_dialog.glade"
    initFxViewer gui rates
    onResponse    (dialog_wnd gui) (\_ -> widgetDestroy  (dialog_wnd gui))
    widgetShowAll (dialog_wnd gui)
    return ()   
            
    

showFxViewer ::(WindowClass t_parent) => t_parent ->  Session -> IO ()
showFxViewer parent session = do
    mServResp <- showWaitDialog parent session GetCurrencyRates
    case mServResp of
        Nothing                    -> do
            dialog <- messageDialogNew Nothing [DialogModal] MessageInfo  ButtonsOk ("Запрос был отменен.")
            windowSetTransientFor dialog parent
            dialogRun dialog
            widgetDestroy dialog
        Just (CurrencyRates rates) -> showFxViewerDialog rates
        Just (Error msg          ) -> do
            dialog <- messageDialogNew Nothing [DialogModal] MessageError ButtonsClose ("Ошибка: " ++ msg ++ ".")
            windowSetTransientFor dialog parent
            dialogRun dialog
            widgetDestroy dialog
        otherwise                  ->  do
            dialog <- messageDialogNew Nothing [DialogModal] MessageError ButtonsClose ("Сервер ответил неверно: ")
            messageDialogSetSecondaryText dialog (show mServResp)
            windowSetTransientFor dialog parent
            dialogRun dialog
            widgetDestroy dialog
    
    
    
   

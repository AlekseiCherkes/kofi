
module AccountChooser where

import Data.List ( isPrefixOf )
import Data.Char ( toLower )
import Data.IORef

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import System.Glib.Signals (on)

-- Common imports
import Types

--Client imports
import GtkCommon
import ClientEntities


type MatchFunc  a = (a -> String -> Bool)
type SelectFunc a = (a -> IO())


data AccChooserDialog = AccChooserDialog { accChooser_dlg :: Dialog
                                         , banks_tv       :: TreeView
                                         , accounts_tv    :: TreeView
                                         , pickAcc_btn    :: Button
                                         , pickNoAcc_btn  :: Button
                                         , selected_acc   :: IORef (Maybe ACC ) 
                                         , selected_bnk   :: IORef (Maybe Bank)
                         }


loadAccChooser :: FilePath -> IO AccChooserDialog
loadAccChooser gladePath = do
    Just glade <- xmlNew gladePath

    accChooser_dlg               <-       xmlGetWidget glade castToDialog     "accChooser_dlg"
    [banks_tv, accounts_tv]      <- mapM (xmlGetWidget glade castToTreeView) ["banks_tv"   , "accounts_tv"  ]
    [bickAcc_btn, pickNoAcc_btn] <- mapM (xmlGetWidget glade castToButton  ) ["pickAcc_btn", "pickNoAcc_btn"]

    acc <- (newIORef Nothing) 
    bnk <-  (newIORef Nothing)
    return $ AccChooserDialog accChooser_dlg banks_tv accounts_tv bickAcc_btn pickNoAcc_btn acc bnk


initAccChooser :: Session -> AccChooserDialog -> IO()
initAccChooser session gui = do
    -- let unp = sessionUnp session
    --writeIORef (selected_bic gui) (Nothing)         
           
    bnkModel <- listStoreNew ([]::[Bank])
    accModel <- listStoreNew ([]::[ACC ])
    
    let onBankSelected = \bank -> do
        putStrLn $ show bank
        writeIORef (selected_bnk gui) (Just bank)
        refillListStore accModel ["1234567890123", "9876543210987"] 
        --writeIORef (selected_acc gui) (Nothing)

    let onAccSelected = \acc -> do
        putStrLn "AccountSelected"
        writeIORef (selected_acc gui) (Just $ acc)
      
    initBanksTreeView     bankDoesMatch onBankSelected (banks_tv    gui) bnkModel
    initAccountsTreeView  accDoesMatch  onAccSelected  (accounts_tv gui) accModel

    let banks = [Bank { bnkBic = "001", bnkName = "Альфа Банк"      }
                ,Bank { bnkBic = "002", bnkName = "Приор Банк"      }
                ,Bank { bnkBic = "003", bnkName = "БелАгроПром Банк"}
                ,Bank { bnkBic = "004", bnkName = "ВТБ Банк"        }
                ,Bank { bnkBic = "005", bnkName = "БПС Банк"        }
                ,Bank { bnkBic = "006", bnkName = "БелСвис Банк"    }
                ,Bank { bnkBic = "007", bnkName = "Белинвест Банк"  }]
    
    refillListStore bnkModel banks  
    

getChoosedAccountPk :: AccChooserDialog -> IO ( Maybe (AccountPK, Name) )
getChoosedAccountPk gui = do
    bnk <- (readIORef $ selected_bnk gui)
    acc <- (readIORef $ selected_acc gui)
    putStrLn ("Selection:" ++ show bnk ++ " " ++ show acc)
    return $ fetchChoosedAccount  bnk acc
    where fetchChoosedAccount Nothing     _          = Nothing
          fetchChoosedAccount _           Nothing    = Nothing
          fetchChoosedAccount (Just bank) (Just acc) = Just (AccountPK acc $ bnkBic bank, bnkName bank)


bankDoesMatch :: MatchFunc Bank
bankDoesMatch bank str = map toLower str `isPrefixOf` map toLower (bnkBic bank)

accDoesMatch :: MatchFunc ACC
accDoesMatch acc str = str `isPrefixOf` acc     



initBanksTreeView :: MatchFunc Bank -> SelectFunc Bank -> TreeView -> ListStore Bank -> IO()
initBanksTreeView match select view model = do
    treeViewSetModel view model
    treeViewSetHeadersVisible view True 
    
    col1 <- makeTextTreeViewColumn "BIC банка"      bnkBic  model
    col2 <- makeTextTreeViewColumn "Название банка" bnkName model
    treeViewAppendColumn view col1
    treeViewAppendColumn view col2

    -- enable interactive search
    treeViewSetEnableSearch view True
    treeViewSetSearchEqualFunc view $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath  model iter
        row   <- listStoreGetValue model i
        return $ match row str 

    -- selection handling is hardcoded to ListStore  
    on view cursorChanged $ do
        ((i:_), _) <- treeViewGetCursor view
        row <- listStoreGetValue model i
        select row
    return ()



initAccountsTreeView :: MatchFunc ACC -> SelectFunc ACC -> TreeView -> ListStore ACC -> IO()
initAccountsTreeView match select view model = do
    putStrLn "Initializing accounts..."
    treeViewSetModel view model
    treeViewSetHeadersVisible view True
    col <- makeTextTreeViewColumn "Номер Счета" id model
    treeViewAppendColumn view col

    -- enable interactive search
    treeViewSetEnableSearch view True
    treeViewSetSearchEqualFunc view $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath  model iter
        row   <- listStoreGetValue model i
        return $ match row str 

    -- selection handling is hardcoded to ListStore  
    on view cursorChanged $ do
        ((i:_), _) <- treeViewGetCursor view
        row <- listStoreGetValue model i
        select row
    
    return ()

    
               

showAccountChooser :: (WindowClass twin)=> Session -> twin -> IO (Maybe (AccountPK, Name))
showAccountChooser session parent = do
    gui <- loadAccChooser "Resources/accountChooser_dialog.glade"
    initAccChooser session gui
    windowSetTransientFor (accChooser_dlg gui) parent 
    responce <- dialogRun (accChooser_dlg gui)
    putStrLn "Responce received"
    widgetDestroy (accChooser_dlg gui)
    
    if responce == ResponseOk 
        then getChoosedAccountPk gui
        else return Nothing
        

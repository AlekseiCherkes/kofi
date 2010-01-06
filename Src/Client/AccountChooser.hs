
module AccountChooser where

import Data.List ( isPrefixOf )
import Data.Char ( toLower )
import Data.IORef
import Control.Monad (liftM2)

-- Gtk imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- Common imports
import Types


--Client imports
import GtkCommon
import ClientEntities
import DataModel



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


initAccChooser :: Session -> UNP -> AccChooserDialog -> IO()
initAccChooser session unp gui = do
    let path = sessionPath session         
           
    bnkModel <- listStoreNew ([]::[Bank])
    accModel <- listStoreNew ([]::[ACC ])
    
    initTreeViewColumns (banks_tv gui) bnkModel [
        ("BIC банка"     , bic2str.bnkBic ),
        ("Название банка", bnkName        )]
        
    initTreeViewColumns (accounts_tv gui) accModel [
        ("Номер Счета", acc2str)]
        
    let onBankSelected = \bank -> do
        writeIORef (selected_acc gui) (Nothing)
        writeIORef (selected_bnk gui) (Just bank)
        accs <- findAccountsByCompanyAndBank path unp (bnkBic bank) 
        refillListStore accModel $ map (accId . accPk) accs
        validateAccountChooser gui
        
    let onAccSelected = \acc -> do
        putStrLn "AccountSelected"
        writeIORef (selected_acc gui) (Just acc)
        validateAccountChooser gui
      
    bindTreeViewHandlers  bankDoesMatch onBankSelected (banks_tv    gui) bnkModel
    bindTreeViewHandlers  accDoesMatch  onAccSelected  (accounts_tv gui) accModel
    
    banks <- findBanksByCompany path unp
    refillListStore bnkModel banks  
    

validateAccountChooser :: AccChooserDialog -> IO ()
validateAccountChooser gui = do
    let isSet = \getter -> isRefSet $ getter gui
    let andM  = liftM2 (&&)
    setButtonSensitive (pickAcc_btn gui) =<< isSet selected_bnk `andM` isSet selected_acc    

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
bankDoesMatch str bank  = map toLower str `isPrefixOf` map toLower (bic2str $ bnkBic bank)

accDoesMatch :: MatchFunc ACC
accDoesMatch str acc  = str `isPrefixOf` acc2str acc     



showAccountChooser :: (WindowClass twin)=> twin -> Session -> UNP -> IO (Maybe (AccountPK, Name))
showAccountChooser parent session unp  = do
    gui <- loadAccChooser "Resources/accountChooser_dialog.glade"
    initAccChooser session unp gui
    validateAccountChooser gui
    windowSetTransientFor (accChooser_dlg gui) parent 
    responce <- dialogRun (accChooser_dlg gui)
    putStrLn "Responce received"
    widgetDestroy (accChooser_dlg gui)
    
    if responce == ResponseOk 
        then getChoosedAccountPk gui
        else return Nothing
        

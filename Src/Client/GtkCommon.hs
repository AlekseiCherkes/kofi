module GtkCommon where

import Control.Monad ((>=>))
import System.IO
import Data.IORef


import Graphics.UI.Gtk
import System.Glib.Signals (on)
import System.Time


import Types
import ClientEntities


type MatchFunc  a = (String -> a -> Bool)
type SelectFunc a = (a -> IO())

-- The purpose of this module is to reduce copypaste application in client files

setComboEntryItems :: ComboBoxEntry -> [String] -> IO ()
setComboEntryItems combo items = do
    comboBoxEntrySetModelText combo
    mapM_ (comboBoxAppendText combo) items
    comboBoxSetActive combo 0


setMultilineText :: TextView -> String -> IO ()
setMultilineText textView text = do
    buffer <- textViewGetBuffer textView
    textBufferSetText buffer text


getMultilineText :: TextView -> IO String
getMultilineText textView = do
	buffer <- textViewGetBuffer textView
	start  <- textBufferGetStartIter buffer
	end    <- textBufferGetEndIter   buffer
	txt    <- textBufferGetText      buffer start end True
	return txt


makeTextTreeViewColumn :: ListStore t_row
                       -> String
                       -> (t_row -> String)
                       -> IO TreeViewColumn
makeTextTreeViewColumn model title row2str  = do
    col <- treeViewColumnNew
    treeViewColumnSetTitle col title
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer True
    cellLayoutSetAttributes col renderer model $ \row -> [ cellText := row2str row ]
    return col


initTreeViewColumns :: TreeView -> ListStore a -> [(String, a -> String)] -> IO ()
initTreeViewColumns view model cols = do
    treeViewSetModel view model
    treeViewSetHeadersVisible view True

    mapM_ (
        (\(title, row2str) -> makeTextTreeViewColumn model title row2str ) >=>
        (treeViewAppendColumn view)
     ) cols


bindTreeViewHandlers :: MatchFunc a -> SelectFunc a -> TreeView -> ListStore a -> IO ()
bindTreeViewHandlers match select view model = do
    -- enable interactive search
    treeViewSetEnableSearch view True
    treeViewSetSearchEqualFunc view $ Just $ \str iter -> do
        (i:_) <- treeModelGetPath  model iter
        row   <- listStoreGetValue model i
        return $ match str row

    -- selection handling is hardcoded to ListStore
    on view cursorChanged $ do
        ((i:_), _) <- treeViewGetCursor view
        row <- listStoreGetValue model i
        select row
    return ()



refillListStore :: ListStore a -> [a] -> IO ()
refillListStore store newItems = do
    listStoreClear store
    mapM_ (listStoreAppend store) newItems


showDate :: CalendarTime -> String
showDate d = (show $ ctYear d) ++ " " ++ (show $ ctMonth d) ++ " " ++ (show $ ctDay d)


renderProfileInfo :: Maybe Profile -> Label -> Label -> Label -> IO ()
renderProfileInfo mprof name_l unp_l date_l = do
    case mprof of
        Nothing   -> do
            labelSetText unp_l  "N/A"
            labelSetText name_l "N/A"
            labelSetText date_l "N/A"
        Just prof -> do
            labelSetText unp_l  $ (unp2str . profileUnp )   prof
            labelSetText name_l $  profileName              prof
            labelSetText date_l $ (showDate. profileDate)   prof


renderAccountInfo :: Maybe (AccountPK, Name) -> Label -> Label -> Label -> IO ()
renderAccountInfo mpair bnk_l bic_l acc_l = do
    case mpair of
        Nothing -> do
            labelSetText bnk_l "N/A"
            labelSetText bic_l "N/A"
            labelSetText acc_l "N/A"
        Just (AccountPK accid ccbic, name) -> do
            labelSetText bnk_l name
            labelSetText bic_l (bic2str ccbic)
            labelSetText acc_l (acc2str accid)


isRefSet :: IORef (Maybe a) -> IO Bool
isRefSet ref = do
    val <- readIORef ref
    case val of
        Nothing -> return False
        Just _  -> return True


setButtonSensitive :: Button -> Bool -> IO ()
setButtonSensitive button condition = set button [widgetSensitive := condition]

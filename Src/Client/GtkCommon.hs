module GtkCommon where

import Control.Monad
import System.IO

import Graphics.UI.Gtk
import System.Glib.Signals (on)


type MatchFunc  a = (a -> String -> Bool)
type SelectFunc a = (a -> IO())



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
        return $ match row str 

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

module GtkCommon where

import System.IO
import Graphics.UI.Gtk



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


makeTextTreeViewColumn :: (TreeModelClass (t_model t_row), TypedTreeModelClass t_model) 
                       => String 
                       -> (t_row -> String) 
                       -> t_model t_row
                       -> IO TreeViewColumn
makeTextTreeViewColumn title row2str model = do
    col <- treeViewColumnNew
    treeViewColumnSetTitle col title
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer True
    cellLayoutSetAttributes col renderer model $ \row -> [ cellText := row2str row ]
    return col

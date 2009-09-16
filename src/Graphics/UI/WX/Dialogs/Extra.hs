
module Graphics.UI.WX.Dialogs.Extra (optionDialog, multiOptionsDialog) where

import Data.List
import Graphics.UI.WX
import Graphics.UI.WX.Dialogs
import Graphics.UI.WXCore
import Graphics.UI.WXCore.WxcClasses

-- | Retrieve an option from a list. Returns 'Nothing' on cancel. Usage:
--
-- > optionsDialog window message caption options selectedOption
--
optionDialog :: (Eq t, Show t) => Window a -> String -> String -> [t] -> Maybe t -> IO (Maybe t)
optionDialog win message caption options selectedOption =
    do
        dlg <- dialog win [text := caption]
        btnok <- button dlg [text := "Ok", identity := wxID_OK]
        btnnok <- button dlg [text := "Cancel", identity := wxID_CANCEL]
        cboOpts <- comboBoxEx dlg wxCB_READONLY [items := map show options]
        case selectedOption of
            Nothing ->
                set cboOpts [selection := -1]
            Just option ->
                case elemIndex option options of
                    Nothing ->
                        set cboOpts [selection := -1]
                    Just ind ->
                        set cboOpts [selection := ind]
        let cboL  = fill $ column 5 [margin 5 $ label message, fill $ widget cboOpts]
            btnsL = margin 5 $ floatRight $ row 5 [widget btnnok, widget btnok]  
        set dlg [layout := fill $ column 5 [cboL, btnsL]]
        showModal dlg $ \stopFun -> do
                                        focusOn cboOpts
                                        set btnok [on command := get cboOpts selection >>= stopFun . Just . (options !!)]
                                        set btnnok [on command := stopFun Nothing]

-- | Retrieve a list of options from a list of possible ones. Returns 'Nothing' on cancel. Usage:
--
-- > multiOptionsDialog window message caption options selectedOptions
--
multiOptionsDialog :: (Eq t, Show t) => Window a -> String -> String -> [t] -> [t] -> IO (Maybe [t])
multiOptionsDialog win message caption options selectedOptions =
    do
        dlg <- dialog win [text := caption]
        btnok <- button dlg [text := "Ok", identity := wxID_OK]
        btnnok <- button dlg [text := "Cancel", identity := wxID_CANCEL]
        lstOpts <- multiListBox dlg [items := map show options]
        let selIndexes = foldr (\option acc ->
                                    case elemIndex option options of
                                        Nothing -> acc
                                        Just ind -> ind : acc) [] selectedOptions
        set lstOpts [selections := selIndexes]
        let cboL  = fill $ column 5 [margin 5 $ label message, fill $ widget lstOpts]
            btnsL = margin 5 $ floatRight $ row 5 [widget btnnok, widget btnok]  
        set dlg [layout := fill $ column 5 [cboL, btnsL]] 
        showModal dlg $ \stopFun -> do
                                        focusOn lstOpts
                                        set btnok [on command := get lstOpts selections >>= stopFun . Just . map (options !!)]
                                        set btnnok [on command := stopFun Nothing]

module HPage.GUI.Dialogs where

import System.FilePath
import Data.List
import Control.Monad.Error
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.WXCore.WxcClasses
import qualified HPage.Control as HP

data Preferences = Prefs {languageExtensions :: [HP.Extension],
                          sourceDirs :: [FilePath],
                          ghcOptions :: String}
    deriving (Eq, Show)

preferencesDialog :: Window a -> String -> Preferences -> IO (Maybe Preferences)
preferencesDialog win caption currentPrefs =
    do
        let availExts = sort HP.availableExtensions
        dlg <- dialog win [text := caption]
        btnok <- button dlg [text := "Ok", identity := wxID_OK]
        btnnok <- button dlg [text := "Cancel", identity := wxID_CANCEL]
        
        lstExts <- multiListBox dlg [items := map show availExts]
        let selIndexes = foldr (\option acc ->
                                    case elemIndex option availExts of
                                        Nothing -> acc
                                        Just ind -> ind : acc) [] $ languageExtensions currentPrefs
        set lstExts [selections := selIndexes]
        
        lstDirs <- singleListBox dlg [style := wxLB_NEEDED_SB, items := sourceDirs currentPrefs]
        btnadd <- button dlg [text := "+", on command := addDir win lstDirs]
        btndel <- button dlg [text := "-", on command := delDir lstDirs]

        txtGhcOld <- textEntry dlg [text := ghcOptions currentPrefs, style := wxTE_READONLY]
        txtGhcNew <- textEntry dlg [text := ""]
                
        let lesL = fill $ boxed "Extensions" $ fill $ widget lstExts
            sdsL = fill $ boxed "Source Dirs" $ fill $ column 2 [fill $ widget lstDirs,
                                                                 floatRight $ row 5 [widget btnadd, widget btndel]]
            gosL = fill $ boxed "Ghc Options" $ fill $ row 0 [widget txtGhcOld, fill $ widget txtGhcNew]
            btnsL = margin 5 $ floatRight $ row 5 [widget btnnok, widget btnok]  
        set dlg [layout := fill $ column 5 [lesL, sdsL, gosL, btnsL]] 
        showModal dlg $ \stopFun -> do
                                        focusOn lstExts
                                        set btnok [on command := getCurrentPrefs lstExts lstDirs txtGhcNew >>= stopFun . Just]
                                        set btnnok [on command := stopFun Nothing]
    where getCurrentPrefs e d g = do
                                    let availExts = sort HP.availableExtensions
                                    les_ <- get e selections
                                    let les = map (availExts !!) les_
                                    sds <- get d items
                                    gos <- get g text
                                    return $ Prefs les sds gos
          addDir w lstDirs = do
                                res <- dirOpenDialog w False "Choose directory to add" ""
                                case res of
                                    Nothing ->
                                        return ()
                                    Just dir ->
                                        itemAppend lstDirs dir
          delDir lstDirs = do
                                sel <- get lstDirs selection
                                case sel of
                                    -1 ->
                                        return ()
                                    it ->
                                        itemDelete lstDirs it
                                        
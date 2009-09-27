
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

data Result = SetPrefs Preferences | LoadPrefs FilePath

preferencesDialog :: Window a -> String -> Preferences -> IO (Maybe Result)
preferencesDialog win caption currentPrefs =
    do
        let availExts = sort HP.availableExtensions
        dlg <- dialog win [text := caption]
        btnok <- button dlg [text := "Ok", identity := wxID_OK]
        buttonSetDefault btnok
        btnimport <- button dlg [text := "Import", identity := wxID_OPEN, tooltip := "Import settings from a setup-config file"]
        btnnok <- button dlg [text := "Cancel", identity := wxID_CANCEL]
        
        lstExts <- multiListBox dlg [items := map show availExts]
        let selIndexes = foldr (\option acc ->
                                    case elemIndex option availExts of
                                        Nothing -> acc
                                        Just ind -> ind : acc) [] $ languageExtensions currentPrefs
        set lstExts [selections := selIndexes]
        
        lstDirs <- singleListBox dlg [style := wxLB_NEEDED_SB, items := sourceDirs currentPrefs]
        btnadd <- smallButton dlg [text := "+", on command := addDir win lstDirs, tooltip := "Add a directory to search for haskell sources"]
        btndel <- smallButton dlg [text := "-", on command := delDir lstDirs, tooltip := "Remove the selected directory"]

        txtGhcOld <- textEntry dlg [text := ghcOptions currentPrefs, style := wxTE_READONLY]
        txtGhcNew <- textEntry dlg [text := ""]
                
        let lesL = fill $ boxed "Extensions" $ fill $ widget lstExts
            sdsL = fill $ boxed "Source Dirs" $ fill $ row 5 [fill $ widget lstDirs,
                                                              column 5 [widget btnadd, expand $ widget btndel]]
            gosL = fill $ boxed "Ghc Options" $ fill $ grid 5 5 [[label "Applied", fill $ widget txtGhcOld],
                                                                 [label "New", fill $ widget txtGhcNew]]
            btnsL = margin 5 $ floatRight $ row 5 [widget btnimport, widget btnnok, widget btnok]  
        set dlg [layout := fill $ column 5 [lesL, sdsL, gosL, btnsL],
                 clientSize := sz 500 300] 
        showModal dlg $ \stopFun -> do
                                        focusOn lstExts
                                        buttonOnCommand btnimport $ openSetupFile dlg >>= stopFun 
                                        buttonOnCommand btnok $ getCurrentPrefs lstExts lstDirs txtGhcNew >>= stopFun . Just . SetPrefs
                                        buttonOnCommand btnnok $ stopFun Nothing
    where getCurrentPrefs e d g = do
                                    let availExts = sort HP.availableExtensions
                                    les_ <- get e selections
                                    let les = map (availExts !!) les_
                                    sds <- get d items
                                    gos <- get g text
                                    return $ Prefs les sds gos
          openSetupFile dlg = do
                            res <- fileOpenDialog dlg True True "Select the setup-config file for your project..."
                                                  [("setup-config",["setup-config"])] "dist" "setup-config"
                            case res of
                                Nothing ->
                                    return Nothing
                                Just setupConfig ->
                                    return . Just $ LoadPrefs setupConfig
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
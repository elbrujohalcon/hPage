
module HPage.GUI.Dialogs where

import Data.List
import Graphics.UI.WX
import Graphics.UI.WXCore
import HPage.GUI.IDs
import qualified HPage.Control as HP
import HPage.Utils.Log

data Preferences = Prefs {languageExtensions :: [HP.Extension],
                          sourceDirs :: [FilePath],
                          ghcOptions :: String}
    deriving (Eq, Show)

aboutDialog :: Window a -> FilePath -> IO ()
aboutDialog win aboutFile =
    htmlDialog win "About \955Page" (sz 400 500) aboutFile

hayooDialog :: Window a -> String -> IO ()
hayooDialog win query =
    htmlDialog win "Hayoo!" (sz 640 480) $ "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query=" ++ query

htmlDialog :: Window a -> String -> Size -> String -> IO ()
htmlDialog win caption winsize url =
    do
        dlg <- dialog win [text := caption]
        htmlw <- htmlWindowCreate dlg idAny (rect (point 0 0) winsize) 0 ""
        debugIO ("url:", url)
        True <- htmlWindowLoadPage htmlw url
        set dlg [layout := fill $ widget htmlw,
                 visible := True,
                 clientSize := winsize]
        windowCenter dlg wxCENTRE_ON_SCREEN
        return ()

preferencesDialog :: Window a -> String -> Preferences -> IO (Maybe Preferences)
preferencesDialog win caption currentPrefs =
    do
        let availExts = sort HP.availableExtensions
        dlg <- dialog win [text := caption]
        btnok <- button dlg [text := "Ok", identity := wxId_OK]
        buttonSetDefault btnok
        btnnok <- button dlg [text := "Cancel", identity := wxId_CANCEL]
        
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
            btnsL = margin 5 $ floatRight $ row 5 [widget btnnok, widget btnok]  
        set dlg [layout := fill $ column 5 [lesL, sdsL, gosL, btnsL],
                 clientSize := sz 500 300] 
        showModal dlg $ \stopFun -> do
                                        focusOn lstExts
                                        buttonOnCommand btnok $ getCurrentPrefs lstExts lstDirs txtGhcNew >>= stopFun . Just
                                        buttonOnCommand btnnok $ stopFun Nothing
    where getCurrentPrefs e d g = do
                                    let availExts = sort HP.availableExtensions
                                    les' <- get e selections
                                    let les = map (availExts !!) les'
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
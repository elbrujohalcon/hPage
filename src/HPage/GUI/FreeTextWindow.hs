{-# LANGUAGE GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             FunctionalDependencies,
             UndecidableInstances,
             ScopedTypeVariables #-}
             
module HPage.GUI.FreeTextWindow ( gui ) where

import Prelude hiding (catch)
import Control.Exception
import Control.Concurrent.Process
import Control.Concurrent.MVar
import System.FilePath
import System.Directory
import System.IO.Error hiding (try, catch)
import System.Exit
import System.Cmd
import Data.List
import Data.Bits
import Data.Char (toLower)
import Data.Version
import Distribution.Package
import Control.Monad.Error
import Control.Monad.Loops
import Graphics.UI.WX
import Graphics.UI.WXCore hiding (kill, Process)
import qualified HPage.Control as HP
import qualified HPage.Server as HPS
import qualified HPage.GUI.SplashScreen as SS
import HPage.GUI.Dialogs
import HPage.GUI.IDs
import HPage.GUI.Constants
import HPage.Utils.Log
import System.Environment.FindBin
import Paths_hpage

imageFile :: FilePath -> IO FilePath
imageFile fp = do
                progPath <- getProgPath
                path <- case takeBaseName progPath of
                            "MacOS" ->
                                return $ dropFileName progPath </> "Resources" </> (takeFileName fp)
                            _ ->
                                getDataFileName $ "res" </> "images" </> fp
                real <- doesFileExist path
                if real then return path
                        else do
                                errorIO ("file not found", path)
                                fail (path ++ " does not exist")

helpFile :: IO FilePath
helpFile =
    do
        progPath <- getProgPath
        case takeBaseName progPath of
            "MacOS" ->
                return $ dropFileName progPath </> "Resources" </> "helpPage.hs"
            _ ->
                getDataFileName $ "res" </> "help" </> "helpPage.hs"
                
aboutFile :: IO FilePath
aboutFile =
    do
        progPath <- getProgPath
        case takeBaseName progPath of
            "MacOS" ->
                return $ dropFileName progPath </> "Resources" </> "about.html"
            _ ->
                getDataFileName $ "res" </> "help" </> "about.html"

data GUIBottom = GUIBtm { bottomDesc :: String,
                          _bottomSource :: String }

data GUIResults = GUIRes { resButton :: Button (),
                           resLabel :: StaticText (),
                           resValue :: TextCtrl (),
                           res4Dots :: StaticText (),
                           resType  :: TextCtrl (),
                           resErrors :: Var [GUIBottom] }

data GUIContext = GUICtx { guiWin       :: Frame (),
                           guiPages     :: SingleListBox (),
                           guiModules   :: (Var Int, ListCtrl ()),
                           guiCode      :: TextCtrl (),
                           guiResults   :: GUIResults,
                           guiStatus    :: StatusField,
                           guiTimer     :: TimerEx (),
                           guiCharTimer :: TimerEx (),
                           guiSearch    :: FindReplaceData (),
                           guiChrVar    :: MVar (Maybe String),
                           guiChrFiller :: MVar (Handle String),
                           guiValFiller :: MVar (Handle (String, IO ()))} 

gui :: [String] -> IO ()
gui args =
    do
        win <- frame [text := "\955Page",
                      visible := False]

        iconFile <- imageFile $ "icon" </> "hpage" <.> "ico"
	iconCreateFromFile iconFile sizeNull >>= topLevelWindowSetIcon win
        
        ssh <- SS.start win
        
        SS.step ssh 0 "Checking installation..."
        
        checkResult <- catch (rawSystem "cabal" ["--version"])
                             (\(_ :: SomeException) -> return $ ExitFailure 1)
        
        debugIO ("result", checkResult)
        
        case checkResult of
            ExitSuccess -> do
                SS.step ssh 10 "Starting the hint-server..."
                
                -- Server context
                model <- HPS.start
                
                SS.step ssh 20 "Starting up..."
                
                set win [on closing := HPS.stop model >> propagateEvent]
        
                -- Containers
                ntbkL <- notebook win []
                pnlPs <- panel ntbkL []
                pnlMs <- panel ntbkL []
                
                -- Text page...
            --  txtCode <- styledTextCtrl win []
                txtCode <- textCtrl win [font := fontFixed, text := ""]
                
                -- Document Selector
                lstPages <- singleListBox pnlPs [style := wxLB_NEEDED_SB, outerSize := sz 400 600]
                
                -- Modules Lists
                imageFiles <- mapM imageFile ["m_imported.ico", "m_interpreted.ico", "m_compiled.ico", "m_package.ico"]
                imagePaths <- mapM getAbsoluteFilePath imageFiles
                images     <- imageListFromFiles (sz 16 16) imagePaths
                varModsSel <- varCreate $ -1
                lstModules <- listCtrlEx pnlMs (wxLC_REPORT + wxLC_ALIGN_LEFT + wxLC_NO_HEADER + wxLC_SINGLE_SEL)
                                               [columns := [("Module", AlignLeft, 200),
                                                            ("Origin", AlignLeft, 1)]]
                listCtrlSetImageList lstModules images wxIMAGE_LIST_SMALL
        
                -- Results panel
                pnlRes <- panel win []
                txtValue <- textEntry pnlRes [style := wxTE_READONLY]
                varErrors <- varCreate []
                txtType <- textEntry pnlRes [style := wxTE_READONLY]
                btnInterpret <- button pnlRes [text := "Interpret"]
                lblInterpret <- staticText pnlRes [text := "Value:"]
                lbl4Dots <- staticText pnlRes [text := " :: "]
                set pnlRes [layout := fill $
                                        row 5 [widget btnInterpret,
                                               centre $ widget lblInterpret,
                                               fill $ widget txtValue,
                                               centre $ widget lbl4Dots,
                                               fill $ widget txtType]]
        
                -- Status bar...
                status <- statusField [text := "hello... this is \955Page! type in your instructions :)"]
                set win [statusBar := [status]]
        
                -- Timers ...
                refreshTimer <- timer win []
                charTimer <- timer win []
                
                -- Search ...
                search <- findReplaceDataCreate wxFR_DOWN
        
                chv <- newEmptyMVar
                chfv <- newEmptyMVar
                vfv <- newEmptyMVar
                
                let guiRes = GUIRes btnInterpret lblInterpret txtValue lbl4Dots txtType varErrors
                let guiCtx = GUICtx win lstPages (varModsSel, lstModules) txtCode guiRes status refreshTimer charTimer search chv chfv vfv 
                let onCmd name acc = traceIO ("onCmd", name) >> acc model guiCtx
        
                -- Helper processes
                chf <- spawn $ charFiller guiCtx
                putMVar chfv chf
                vf <- spawn $ valueFiller guiCtx
                putMVar vfv vf
                
                -- Events
                timerOnCommand refreshTimer $ refreshExpr model guiCtx
                timerOnCommand charTimer $ do
                                             wasEmpty <- tryPutMVar chv Nothing
                                             if wasEmpty
                                                 then do
                                                     newchf <- spawn $ charFiller guiCtx
                                                     swapMVar chfv newchf >>= kill
                                                 else return ()
        
                set btnInterpret [on command := onCmd "interpret" interpret]
                
                set lstPages [on select := onCmd "pageChange" pageChange]
                set txtCode [on keyboard := onCmd "key" . keyEvent,
                             on mouse :=  \e -> case e of
                                                    MouseLeftUp _ _ -> onCmd "mouseEvent" restartTimer >> propagateEvent
                                                    MouseLeftDClick _ _ -> onCmd "mouseEvent" restartTimer >> propagateEvent
                                                    MouseRightDown _ _ -> onCmd "textContextMenu" textContextMenu
                                                    _ -> propagateEvent]
                set txtValue [on mouse := \e -> case e of
                                                    MouseRightDown _ _ -> onCmd "valueContextMenu" valueContextMenu
                                                    _ -> propagateEvent]
                set txtType [on mouse := \e -> case e of
                                                    MouseRightDown _ _ -> onCmd "typeContextMenu" typeContextMenu
                                                    _ -> propagateEvent]
                set lstModules [on listEvent := \e -> case e of
                                                        ListItemSelected idx -> varSet varModsSel idx
                                                        ListItemRightClick idx -> varSet varModsSel idx >> onCmd "moduleContextMenu" moduleContextMenu
                                                        _ -> propagateEvent]
                
                -- Menu bar...
                mnuPage <- menuPane [text := "Page"]
                menuAppend mnuPage wxId_NEW "&New\tCtrl-n" "New Page" False
                menuAppend mnuPage wxId_CLOSE "&Close\tCtrl-w" "Close Page" False
                menuAppend mnuPage wxId_CLOSE_ALL "&Close All\tCtrl-Shift-w" "Close All Pages" False
                menuAppendSeparator mnuPage
                menuAppend mnuPage wxId_OPEN "&Open...\tCtrl-o" "Open Page" False
                menuAppend mnuPage wxId_SAVE "&Save\tCtrl-s" "Save Page" False
                menuAppend mnuPage wxId_SAVEAS "&Save as...\tCtrl-Shift-s" "Save Page as" False
                menuAppendSeparator mnuPage
                _menuQuit <- menuQuit mnuPage [on command := wxcAppExit]
                
                mnuEdit <- menuPane [text := "Edit"]
                menuAppend mnuEdit wxId_UNDO "&Undo\tCtrl-z" "Undo" False
                menuAppend mnuEdit wxId_REDO "&Redo\tCtrl-Shift-z" "Redo" False
                menuAppendSeparator mnuEdit
                menuAppend mnuEdit wxId_CUT "C&ut\tCtrl-x" "Cut" False
                menuAppend mnuEdit wxId_COPY "&Copy\tCtrl-c" "Copy" False
                menuAppend mnuEdit wxId_PASTE "&Paste\tCtrl-v" "Paste" False
                menuAppendSeparator mnuEdit
                menuAppend mnuEdit wxId_FIND "&Find...\tCtrl-f" "Find" False
                menuAppend mnuEdit wxId_FORWARD "Find &Next\tCtrl-g" "Find Next" False
                menuAppend mnuEdit wxId_BACKWARD "Find &Previous\tCtrl-Shift-g" "Find Previous" False
                menuAppend mnuEdit wxId_REPLACE "&Replace...\tCtrl-Shift-r" "Replace" False
                menuAppendSeparator mnuEdit
                menuAppend mnuEdit wxId_PREFERENCES "&Preferences...\tCtrl-," "Preferences" False
        
                mnuHask <- menuPane [text := "Haskell"]
                menuAppend mnuHask wxId_HASK_LOAD_PKG "Load &package...\tCtrl-Alt-l" "Load Cabal Package" False
                menuAppendSeparator mnuHask
                menuAppend mnuHask wxId_HASK_LOAD "&Load modules...\tCtrl-l" "Load Modules" False
                menuAppend mnuHask wxId_HASK_LOADNAME "Load modules by &name...\tCtrl-Shift-l" "Load Modules by Name" False
                menuAppend mnuHask wxId_HASK_ADD "Import modules...\tCtrl-Shift-i" "Import Packaged Modules by Name" False
                menuAppend mnuHask wxId_HASK_RELOAD "&Reload\tCtrl-r" "Reload Modules" False
                menuAppendSeparator mnuHask
                menuAppend mnuHask wxId_HASK_INTERPRET "&Interpret\tCtrl-i" "Interpret the Current Expression" False
                menuAppend mnuHask wxId_HASK_NAVIGATE "Search on Ha&yoo!\tCtrl-y" "Search the Current Selection on Hayoo!" False
                
                mnuHelp <- menuHelp []
                menuAppend mnuHelp wxId_HELP "&Help page\tCtrl-h" "Open the Help Page" False
                about <- aboutFile
                _menuAbout <- menuAbout mnuHelp [on command := aboutDialog win about]
                
                set win [menuBar := [mnuPage, mnuEdit, mnuHask, mnuHelp]]
                evtHandlerOnMenuCommand win wxId_NEW $ onCmd "runHP' addPage" $ runHP' HP.addPage
                evtHandlerOnMenuCommand win wxId_CLOSE $ onCmd "runHP' closePage" $ runHP' HP.closePage
                evtHandlerOnMenuCommand win wxId_CLOSE_ALL $ onCmd "runHP' closeAllPages" $ runHP' HP.closeAllPages
                evtHandlerOnMenuCommand win wxId_OPEN $ onCmd "openPage" openPage
                evtHandlerOnMenuCommand win wxId_SAVE $ onCmd "savePage" savePage
                evtHandlerOnMenuCommand win wxId_SAVEAS $ onCmd "savePageAs" savePageAs
                evtHandlerOnMenuCommand win wxId_UNDO $ onCmd "runHP' undo" $ runHP' HP.undo
                evtHandlerOnMenuCommand win wxId_REDO $ onCmd "runHP' redo" $ runHP' HP.redo
                evtHandlerOnMenuCommand win wxId_CUT $ onCmd "cut" cut
                evtHandlerOnMenuCommand win wxId_COPY $ onCmd "copy" copy
                evtHandlerOnMenuCommand win wxId_PASTE $ onCmd "paste" paste
                evtHandlerOnMenuCommand win wxId_FIND $ onCmd "justFind" justFind
                evtHandlerOnMenuCommand win wxId_FORWARD $ onCmd "findNext" justFindNext
                evtHandlerOnMenuCommand win wxId_BACKWARD $ onCmd "findPrev" justFindPrev
                evtHandlerOnMenuCommand win wxId_REPLACE $ onCmd "findReplace" findReplace
                evtHandlerOnMenuCommand win wxId_HASK_LOAD_PKG $ onCmd "loadPackage" loadPackage
                evtHandlerOnMenuCommand win wxId_HASK_LOAD $ onCmd "loadModules" loadModules
                evtHandlerOnMenuCommand win wxId_HASK_ADD $ onCmd "importModules" importModules
                evtHandlerOnMenuCommand win wxId_HASK_LOADNAME $ onCmd "loadModulesByName" loadModulesByName
                evtHandlerOnMenuCommand win wxId_HASK_LOAD_FAST $ onCmd "loadModulesByNameFast" loadModulesByNameFast
                evtHandlerOnMenuCommand win wxId_HASK_RELOAD $ onCmd "reloadModules" reloadModules
                evtHandlerOnMenuCommand win wxId_PREFERENCES $ onCmd "preferences" configure
                evtHandlerOnMenuCommand win wxId_HASK_INTERPRET $ onCmd "interpret" interpret
                evtHandlerOnMenuCommand win wxId_HASK_NAVIGATE $ onCmd "hayoo" hayoo
                evtHandlerOnMenuCommand win wxId_HASK_COPY $ onCmd "copyResult" copyResult
                evtHandlerOnMenuCommand win wxId_HASK_COPY_TYPE $ onCmd "copyType" copyType
                evtHandlerOnMenuCommand win wxId_HASK_EXPLAIN $ onCmd "explain" explain
                evtHandlerOnMenuCommand win wxId_HELP $ onCmd "help" openHelpPage
                
                -- Tool bar...
                tbMain <- toolBarEx win True True []
                mitLoadPkg <- menuFindItem mnuHask wxId_HASK_LOAD_PKG
                mitNew <- menuFindItem mnuPage wxId_NEW
                mitOpen <- menuFindItem mnuPage wxId_OPEN
                mitSave <- menuFindItem mnuPage wxId_SAVE
                mitCut <- menuFindItem mnuEdit wxId_CUT
                mitCopy <- menuFindItem mnuEdit wxId_COPY
                mitPaste <- menuFindItem mnuEdit wxId_PASTE
                mitReload <- menuFindItem mnuHask wxId_HASK_RELOAD
                loadPath <- imageFile "load.png"
                newPath <- imageFile "new.png"
                openPath <- imageFile "open.png"
                savePath <- imageFile "save.png"
                cutPath <- imageFile "cut.png"
                copyPath <- imageFile "copy.png"
                pastePath <- imageFile "paste.png"
                reloadPath <- imageFile "reload.png"
                _loadPackage <- toolMenu tbMain mitLoadPkg "Load Package" loadPath [tooltip := "Load Cabal Package"]
                toolBarAddSeparator tbMain
                _new <- toolMenu tbMain mitNew "New" newPath [tooltip := "New Page"]
                _open <- toolMenu tbMain mitOpen "Open" openPath [tooltip := "Open Page"]
                _save <- toolMenu tbMain mitSave "Save" savePath [tooltip := "Save Page"]
                toolBarAddSeparator tbMain
                _cut <- toolMenu tbMain mitCut "Cut" cutPath [tooltip := "Cut"]
                _copy <- toolMenu tbMain mitCopy "Copy" copyPath [tooltip := "Copy"]
                _paste <- toolMenu tbMain mitPaste "Paste" pastePath [tooltip := "Paste"]
                toolBarAddSeparator tbMain
                _reload <- toolMenu tbMain mitReload "Reload" reloadPath [tooltip := "Reload Modules"]
                toolBarSetToolBitmapSize tbMain $ sz 32 32
        
                -- Layout settings
                let pagesTabL   = tab "Pages" $ container pnlPs $ fill $ margin 5 $ widget lstPages
                    modsTabL    = tab "Modules" $ container pnlMs $ fill $ margin 5 $ widget lstModules
                    leftL       = tabs ntbkL [modsTabL, pagesTabL]
                    resultsL    = hfill $ boxed "Expression" $ fill $ widget pnlRes
                    rightL      = minsize (sz 485 100) $ fill $ widget txtCode
                set win [layout := column 5 [fill $ row 10 [leftL, rightL], resultsL],
                         clientSize := sz 800 600]
                         
                --HACK: We need to keep a timer ticking just to refresh the screen when the user is doing nothing
                --      That's because the main C loop of wx only calls wxHaskell callbacks when something happens
                --      and we try to make things happen in this side but they're not reflected there until some-
                --      thing happens there
                _tickingTimer <- timer win [interval := 50, on command := return ()]
                
                -- test the server...
                SS.step ssh 40 "Preparing model..."
                Right _ <- runTxtHPSelection "1" model HP.interpret
                
                SS.step ssh 60 "Loading first page..."
                -- ...and RUN!
                refreshPage model guiCtx
                
                SS.step ssh 80 "Loading help page..."
                onCmd "start" openHelpPage
                
                SS.step ssh 90 "Loading UI..."
                case args of
                    [] ->
                        return ()
                    dir:_ ->
                        do
                            SS.step ssh 92 "looking for package files..."
                            setupConfig <- canonicalizePath $ dir </> "dist" </> "setup-config"
                            pkgExists <- doesFileExist setupConfig
                            case pkgExists of
                                False ->
                                    warningDialog win "Error" $ setupConfig ++ " doesn't exist.\n  Maybe you have to reconfigure the package"
                                True -> do
                                    SS.step ssh 95 "Loading package..."
                                    loadres <- tryIn' model $ do
                                                                lr <- HP.loadPackage setupConfig
                                                                HP.addPage
                                                                return lr
                                    case loadres of
                                        Left err ->
                                            warningDialog win "Error" err
                                        Right (Left err) ->
                                            warningDialog win "Error" err
                                        Right (Right pkg) ->
                                            do
                                                setCurrentDirectory dir
                                                SS.step ssh 97 "warming up..."
                                                frameSetTitle win $ "\955Page - " ++ prettyShow pkg
                                    
                                    SS.step ssh 99 "Cleaning UI..."
                                    refreshPage model guiCtx
                
                SS.step ssh 100 "ready"
                set win [visible := True]
                set txtCode [font := fontFixed] -- again just to be sure
                focusOn txtCode
            errRes -> do
                SS.step ssh 100 "failed"
                set win [visible := True]
                shutdownTimer <- timer win [on command := do
                                                                errorDialog win "Error" "Seems like you don't have Cabal installed.\nPlease install the Haskelll Platform from http://hackage.haskell.org/platform/"
                                                                wxcAppExit
                                                                exitWith errRes]
                True <- timerStart shutdownTimer 50 True
                return ()

-- PROCESSES -------------------------------------------------------------------
charFiller :: GUIContext -> Process String ()
charFiller GUICtx{guiResults = GUIRes{resErrors= varErrors},
                  guiChrVar  = chv} =
    forever $ do
         t <- recv
         liftIO $ do
                    txt <- catch (eval t) $ \(ErrorCall desc) ->
                                                    varUpdate varErrors (++ [GUIBtm desc t]) >>
                                                    return bottomChar
                    debugIO $ "Writing chv := " ++ txt
                    tryPutMVar chv $ Just txt
    where eval t = t `seq` length t `seq` return t

valueFiller :: GUIContext -> Process (String, IO ()) ()
valueFiller guiCtx@GUICtx{guiResults   = GUIRes{resButton = btnInterpret,
                                                resErrors = varErrors,
                                                resValue  = txtValue},
                          guiStatus    = status} =
    forever $ do
                liftDebugIO "Waiting for a new value to interpret"
                (val, poc) <- recv
                liftDebugIO "Value received in valueFiller"
                liftDebugIO "Trying to evaluate the whole value first..."
                liftIO $ do
                            set txtValue [text := ""]
                            varSet varErrors []
                            res <- valueFill guiCtx val
                            if res == bottomChar
                                then do
                                        debugIO "didn't work... going char by char..."
                                        varSet varErrors []
                                        statusText <- valueFiller' guiCtx val
                                        errs <- varGet varErrors
                                        case (statusText, errs) of
                                            ("", []) ->
                                                set status [text := ""] >>
                                                set txtValue [enabled := True,
                                                              bgcolor := white]
                                            ("", _) ->
                                                set status [text := "Expression interpreted with errors: Check them by right-clicking on each one"] >>
                                                set txtValue [enabled := True,
                                                              bgcolor := yellow]
                                            (msg, _) ->
                                                set status [text := "Expression interpreted with errors: " ++ msg] >>
                                                set txtValue [enabled := True,
                                                              bgcolor := yellow]
                                else do
                                        debugIO "It worked!!"
                                        textCtrlAppendText txtValue res
                                        set status [text := ""]
                                        set txtValue [enabled := True,
                                                      bgcolor := white]
                            set btnInterpret [on command := poc,
                                              text := "Interpret"]

valueFiller' :: GUIContext -> String -> IO String
valueFiller' guiCtx@GUICtx{guiResults = GUIRes{resValue  = txtValue}} val =
      do
        h <- try (case val of
                      [] -> return []
                      (c:_) -> return [c])
        case h of
            Left (ErrorCall desc) ->
                return desc
            Right [] ->
                return ""
            Right t ->
                do
                    valueFill guiCtx t >>= textCtrlAppendText txtValue
                    valueFiller' guiCtx $ tail val

valueFill :: GUIContext -> String -> IO String
valueFill GUICtx{guiResults = GUIRes{resErrors = varErrors},
                 guiCharTimer = charTimer,
                 guiChrVar    = chv,
                 guiChrFiller = chfv} val =
    do
        debugIO "valueFill starting..."
        _ <- tryTakeMVar chv --NOTE: empty the var
        debugIO "timer starting..."
        True <- timerStart charTimer charTimeout True
        debugIO "sending msg to charFiller..."
        readMVar chfv >>= flip sendTo val
        debugIO "waiting for value toAdd..."
        toAdd <- readMVar chv --NOTE: Not using "take" to be sure that noone touches it
        debugIO ("Ready to add...", toAdd)
        case toAdd of
            Just txt ->
                do
                    isR <- timerIsRuning charTimer
                    if isR then timerStop charTimer else return ()
                    debugIO $ "returning " ++ txt
                    return txt
            Nothing -> --NOTE: Means "Timed Out"
                do
                    _newVal <- varUpdate varErrors (++ [GUIBtm "Timed Out" val])
                    debugIO $ "timed out"
                    return bottomChar

-- EVENT HANDLERS --------------------------------------------------------------
keyEvent :: EventKey -> HPS.ServerHandle -> GUIContext -> IO ()
keyEvent eventKey model guiCtx@GUICtx{guiCode = txtCode} =
    do
        case keyKey eventKey of
            KeyTab ->
                if isNoneDown (keyModifiers eventKey)
                    then
                        textCtrlWriteText txtCode "\t" >> restartTimer model guiCtx
                    else
                        return ()
            _ ->
                restartTimer model guiCtx
        propagateEvent
    
refreshPage, savePageAs, savePage, openPage,
    pageChange, copy, copyResult, copyType, cut, paste,
    justFind, justFindNext, justFindPrev, findReplace,
    textContextMenu, moduleContextMenu, valueContextMenu, typeContextMenu,
    restartTimer, interpret, hayoo, explain,
    loadPackage, loadModules, importModules, loadModulesByName, loadModulesByNameFast, reloadModules,
    configure, openHelpPage :: HPS.ServerHandle -> GUIContext -> IO ()

moduleContextMenu model GUICtx{guiWin = win, guiModules = (varModsSel, lstModules)} =
    do
        pointWithinWindow <- windowGetMousePosition win
        i <- varGet varModsSel
        contextMenu <- menuPane []
        case i of
            (-1) ->
                do
                    return ()
            j ->
                do
                    itm <- get lstModules $ item j
                    case itm of
                        [_, "Package"] ->
                            menuAppend contextMenu wxId_HASK_LOAD_FAST "&Load" "Load Module" False
                        [modname, _] ->
                            appendBrowseMenu contextMenu modname
                        other ->
                            menuAppend contextMenu idAny (show other) "Other" False
                    propagateEvent
                    menuPopup contextMenu pointWithinWindow win
                    objectDelete contextMenu
    where appendBrowseMenu contextMenu mn =
            do
                browseMenu <- menuPane []
                hpsRes <- tryIn model $ HP.getModuleExports mn
                case hpsRes of
                    Left err ->
                        menuAppend browseMenu idAny err "Error" False
                    Right mes ->
                        flip mapM_ mes $ createMenuItem browseMenu
                _copy <- menuItem contextMenu [text := "To Clipboard",
                         		               on command := addToClipboard mn]
                _search <- menuItem contextMenu [text := "Search on Hayoo!",
                           			             on command := hayooDialog win mn]
                menuAppendSeparator contextMenu
                menuAppendSub contextMenu wxId_HASK_BROWSE "&Browse" browseMenu ""
          addToClipboard txt =
            do
                tdo <- textDataObjectCreate txt
                cb <- clipboardCreate
                opened <- clipboardOpen cb
                if opened
                    then do
                        r <- clipboardSetData cb tdo
                        if r
                            then return ()
                            else errorDialog win "Error" "Clipboard operation failed"
                        clipboardClose cb
                    else
                        errorDialog win "Error" "Clipboard not ready"
          createMenuItem m fn@HP.MEFun{HP.funName = fname} =
            do
                itemMenu <- createBasicMenuItem fname
                menuAppendSub m wxId_HASK_MENUELEM (show fn) itemMenu ""
          createMenuItem m HP.MEClass{HP.clsName = cn, HP.clsFuns = []} =
            do
                itemMenu <- createBasicMenuItem cn
                menuAppendSub m wxId_HASK_MENUELEM ("class " ++ cn) itemMenu ""
          createMenuItem m HP.MEClass{HP.clsName = cn, HP.clsFuns = cfs} =
            do
                subMenu <- createBasicMenuItem cn
                menuAppendSeparator subMenu
                flip mapM_ cfs $ createMenuItem subMenu
                menuAppendSub m wxId_HASK_MENUELEM ("class " ++ cn) subMenu ""
          createMenuItem m HP.MEData{HP.datName = dn, HP.datCtors = []} =
            do
                itemMenu <- createBasicMenuItem dn
                menuAppendSub m wxId_HASK_MENUELEM ("data " ++ dn) itemMenu ""
          createMenuItem m HP.MEData{HP.datName = dn, HP.datCtors = dcs} =
            do
                subMenu <- createBasicMenuItem dn
                menuAppendSeparator subMenu
                flip mapM_ dcs $ createMenuItem subMenu
                menuAppendSub m wxId_HASK_MENUELEM ("data " ++ dn) subMenu ""
          createBasicMenuItem name =
            do
                itemMenu <- menuPane []
                _copy <- menuItem itemMenu [text := "To Clipboard",
                                   			on command := addToClipboard name]
                _search <- menuItem itemMenu [text := "Search on Hayoo!",
                           			          on command := hayooDialog win name]
                return itemMenu


textContextMenu _model GUICtx{guiWin = win, guiCode = txtCode} =
    do
        contextMenu <- menuPane []
        sel <- textCtrlGetStringSelection txtCode
        case sel of
                "" ->
                        return ()
                _ ->
                    do
                        menuAppend contextMenu wxId_CUT "C&ut\tCtrl-x" "Cut" False
                        menuAppend contextMenu wxId_COPY "&Copy\tCtrl-c" "Copy" False
                        menuAppend contextMenu wxId_PASTE "&Paste\tCtrl-v" "Paste" False
                        menuAppendSeparator contextMenu
                        menuAppend contextMenu wxId_HASK_NAVIGATE "Search on Ha&yoo!\tCtrl-y" "Search the Current Selection on Hayoo!" False
        menuAppend contextMenu wxId_HASK_INTERPRET "&Interpret\tCtrl-i" "Interpret the Current Expression" False
        propagateEvent
        pointWithinWindow <- windowGetMousePosition win
        menuPopup contextMenu pointWithinWindow win
        objectDelete contextMenu

valueContextMenu _model GUICtx{guiWin = win,
                               guiResults = GUIRes{resValue = txtValue}} =
    do
        contextMenu <- menuPane []
        sel <- textCtrlGetStringSelection txtValue
        case sel of
            "" ->
                return ()
            _ ->
                menuAppend contextMenu wxId_HASK_COPY "Copy" "Copy" False
        if sel == bottomChar || sel == bottomString
            then menuAppend contextMenu wxId_HASK_EXPLAIN "Explain" "Explain" False
            else return ()
        propagateEvent
        pointWithinWindow <- windowGetMousePosition win
        menuPopup contextMenu pointWithinWindow win
        objectDelete contextMenu
        
typeContextMenu _model GUICtx{guiWin = win,
                              guiResults = GUIRes{resType = txtType}} =
    do
        contextMenu <- menuPane []
        sel <- textCtrlGetStringSelection txtType
        case sel of
            "" ->
                do
                    propagateEvent
                    objectDelete contextMenu
            _ ->
                do
                    menuAppend contextMenu wxId_HASK_COPY_TYPE "Copy" "Copy" False
                    propagateEvent
                    pointWithinWindow <- windowGetMousePosition win
                    menuPopup contextMenu pointWithinWindow win
                    objectDelete contextMenu


pageChange model guiCtx@GUICtx{guiPages = lstPages} =
    do
        i <- get lstPages selection
        case i of
            (-1) -> return ()
            _ -> runHP' (HP.setPageIndex i) model guiCtx

openPage model guiCtx@GUICtx{guiWin = win,
                             guiStatus = status} =
    do
        fileNames <- filesOpenDialog win True True "Open file..." [("Haskells",["*.hs"]),
                                                                   ("Any file",["*.*"])] "" ""
        case fileNames of
            [] ->
                return ()
            fs ->
                do
                    set status [text := "opening..."]
                    flip mapM_ fs $ \f -> runHP' (HP.openPage f) model guiCtx

savePageAs model guiCtx@GUICtx{guiWin = win, guiStatus = status} =
    do
        refreshExpr model guiCtx
        fileName <- fileSaveDialog win True True "Save file..." [("Haskells",["*.hs"]),
                                                                 ("Any file",["*.*"])] "" ""
        case fileName of
            Nothing ->
                return ()
            Just f ->
                do
                    set status [text := "saving..."]
                    runHP' (HP.savePageAs f) model guiCtx

savePage model guiCtx@GUICtx{guiWin = win} =
    do
        refreshExpr model guiCtx
        maybePath <- tryIn' model HP.getPagePath
        case maybePath of
            Left err ->
                warningDialog win "Error" err
            Right Nothing ->
                savePageAs model guiCtx
            Right _ ->
                do
                    set (guiStatus guiCtx) [text := "saving..."]
                    runHP' HP.savePage model guiCtx

copy _model GUICtx{guiCode = txtCode} = textCtrlCopy txtCode

copyResult _model GUICtx{guiResults = GUIRes{resValue = txtValue}} = textCtrlCopy txtValue

copyType _model GUICtx{guiResults = GUIRes{resType = txtType}} = textCtrlCopy txtType

cut model guiCtx@GUICtx{guiCode = txtCode} = textCtrlCut txtCode >> refreshExpr model guiCtx

paste model guiCtx@GUICtx{guiCode = txtCode} = textCtrlPaste txtCode >> refreshExpr model guiCtx

justFind model guiCtx = openFindDialog model guiCtx "Find..." dialogDefaultStyle

justFindNext model guiCtx@GUICtx{guiSearch = search} =
    do
        curFlags <- findReplaceDataGetFlags search
        findReplaceDataSetFlags search $ curFlags .|. wxFR_DOWN
        findNextButton model guiCtx

justFindPrev model guiCtx@GUICtx{guiSearch = search} =
    do
        curFlags <- findReplaceDataGetFlags search
        findReplaceDataSetFlags search $ curFlags .&. complement wxFR_DOWN
        findNextButton model guiCtx

findReplace model guiCtx = openFindDialog model guiCtx "Find and Replace..." $ dialogDefaultStyle .|. wxFR_REPLACEDIALOG
        
reloadModules = runHP HP.reloadModules

loadPackage model guiCtx@GUICtx{guiWin = win} =
    do
        distExists <- doesDirectoryExist "dist"
        let startDir = if distExists then "dist" else ""
        res <- fileOpenDialog win True True "Select the setup-config file for your project..."
                              [("setup-config",["setup-config"])] startDir "setup-config"
        case res of
            Nothing ->
                return ()
            Just setupConfig ->
                do
                    loadres <- tryIn' model $ do
                                                lr <- HP.loadPackage setupConfig
                                                HP.addPage
                                                return lr
                    case loadres of
                        Left err ->
                            warningDialog win "Error" err
                        Right (Left err) ->
                            warningDialog win "Error" err
                        Right (Right pkg) ->
                            do
                                absPath <- canonicalizePath setupConfig
                                let dir = joinPath . reverse . drop 2 . reverse $ splitDirectories absPath
                                setCurrentDirectory dir
                                frameSetTitle win $ "\955Page - " ++ prettyShow pkg
                    refreshPage model guiCtx

loadModules model guiCtx@GUICtx{guiWin = win, guiStatus = status} =
    do
        fileNames <- filesOpenDialog win True True "Load Module..." [("Haskell Modules",["*.hs"])] "" ""
        case fileNames of
            [] ->
                return ()
            fs ->
                do
                    set status [text := "loading..."]
                    runHP (HP.loadModules fs) model guiCtx

loadModulesByName model guiCtx@GUICtx{guiWin = win, guiStatus = status} =
    do
        moduleNames <- textDialog win "Enter the module names, separated by spaces" "Load Modules..." ""
        case moduleNames of
            "" ->
                return ()
            mns ->
                do
                    set status [text := "loading..."]
                    runHP (HP.loadModules $ words mns) model guiCtx

loadModulesByNameFast model guiCtx@GUICtx{guiModules = (varModsSel, lstModules),
                                          guiStatus = status} =
    do
        i <- varGet varModsSel
        case i of
            (-1) -> return ()
            j ->
                do
                    mnText <- listCtrlGetItemText lstModules j
                    let mns = [mnText]
                    set status [text := "loading..."]
                    runHP (HP.loadModules mns) model guiCtx

importModules model guiCtx@GUICtx{guiWin = win, guiStatus = status} =
    do
        moduleNames <- textDialog win "Enter the module names, separated by spaces" "Import Packaged Modules..." ""
        case moduleNames of
            "" ->
                return ()
            mns ->
                do
                    set status [text := "loading..."]
                    runHP (HP.importModules $ words mns) model guiCtx

configure model guiCtx@GUICtx{guiWin = win, guiStatus = status} =
    do
        hpsRes <- tryIn model $ do
                                    les <- HP.getLanguageExtensions
                                    sds <- HP.getSourceDirs
                                    gos <- HP.getGhcOpts
                                    case les of
                                        Left e -> return $ Left e
                                        Right l -> return $ Right (l, sds, gos)
        case hpsRes of
            Left err ->
                warningDialog win "Error" err
            Right (les, sds, gos) ->
                do
                    res <- preferencesDialog win "Preferences" $ Prefs les sds gos
                    case res of
                        Nothing ->
                            return ()
                        Just newps ->
                            do
                                set status [text := "setting..."]
                                runHP (do
                                            Right () <- HP.setLanguageExtensions $ languageExtensions newps
                                            Right () <- HP.setSourceDirs $ sourceDirs newps
                                            case ghcOptions newps of
                                                "" -> return $ Right ()
                                                newopts -> HP.setGhcOpts newopts
                                            ) model guiCtx

openHelpPage model guiCtx@GUICtx{guiCode = txtCode} =
    do
        f <- helpFile
        txt <- readFile f
        set txtCode [font := fontFixed,
                     text := txt]
        -- Refresh the current expression box
        refreshExpr model guiCtx

refreshPage model guiCtx@GUICtx{guiWin = win,
                                guiPages = lstPages,
                                guiModules = (varModsSel, lstModules),
                                guiCode = txtCode,
                                guiStatus = status} =
    do
        res <- tryIn' model $ do
                                pc <- HP.getPageCount
                                pages <- mapM HP.getPageNthDesc [0..pc-1]
                                ind <- HP.getPageIndex
                                txt <- HP.getPageText
                                lmsRes <- HP.getLoadedModules
                                ims <- HP.getImportedModules
                                pms <- HP.getPackageModules
                                let lms = case lmsRes of
                                            Left  _ -> []
                                            Right x -> x
                                return (pms, ims, lms, pages, ind, txt)
        case res of
            Left err ->
                warningDialog win "Error" err
            Right (pms, ims, ms, ps, i, t) ->
                do
                    -- Refresh the pages list
                    itemsDelete lstPages
                    (flip mapM_) ps $ \pd ->
                                        let prefix = if HP.pIsModified pd
                                                        then "*"
                                                        else ""
                                            name   = case HP.pPath pd of
                                                         Nothing -> "new page"
                                                         Just fn -> takeFileName $ dropExtension fn
                                         in itemAppend lstPages $ prefix ++ name
                    set lstPages [selection := i]
                    
                    -- Refresh the modules lists
                    --NOTE: we know 0 == "imported" / 1 == "interpreted" / 2 == "compiled" / 3 == "package" images
                    --TODO: move that to some kind of constants or so
                    let ims' = map (\m -> (0, [m, "Imported"])) ims
                        ms' = map (\m -> if HP.modInterpreted m
                                            then (1, [HP.modName m, "Interpred"])
                                            else (2, [HP.modName m, "Compiled"])) ms
                        pms' = map (\m -> (3, [m, "Package"])) $
                                        flip filter pms $ \pm -> all (\xm -> HP.modName xm /= pm) ms
                        allms = zip [0..] (ims' ++ ms' ++ pms')
                    itemsDelete lstModules
                    (flip mapM_) allms $ \(idx, (img, m@(mn:_))) ->
                                                listCtrlInsertItemWithLabel lstModules idx mn img >>                                                
                                                set lstModules [item idx := m]
                    varSet varModsSel $ -1
                    
                    -- Refresh the current text
                    set txtCode [text := t,
                                 font := fontFixed]
                    
                    -- Clean the status bar
                    set status [text := ""]
                    
                    -- Refresh the current expression box
                    refreshExpr model guiCtx

runHP' ::  HP.HPage () -> HPS.ServerHandle -> GUIContext -> IO ()
runHP' a = runHP $ a >>= return . Right

runHP ::  HP.HPage (Either HP.InterpreterError ()) -> HPS.ServerHandle -> GUIContext -> IO ()
runHP hpacc model guiCtx@GUICtx{guiWin = win} =
    do
        res <- tryIn model hpacc
        case res of
            Left err ->
                warningDialog win "Error" err
            Right () ->
                refreshPage model guiCtx

explain _model GUICtx{guiWin = win,
                      guiResults = GUIRes{resValue = txtValue,
                                          resErrors = varErrors}} =
    do
        sel <- textCtrlGetStringSelection txtValue
        if sel == bottomChar || sel == bottomString
            then do
                    txt <- get txtValue text
                    ip <- textCtrlGetInsertionPoint txtValue
                    errs <- varGet varErrors
                    let prevTxt = take ip txt
                        isBottom c = [c] == bottomChar || [c] == bottomString
                        errNo = length $ filter isBottom prevTxt
                        err = if length errs > errNo
                                then bottomDesc $ errs !! errNo
                                else "Unknown"
                    if sel == bottomChar
                        then errorDialog win "Bottom Char" err
                        else errorDialog win "Bottom String" err
            else return ()

hayoo _model GUICtx{guiCode = txtCode, guiWin = win} =
    textCtrlGetStringSelection txtCode >>= hayooDialog win

interpret model guiCtx@GUICtx{guiResults = GUIRes{resLabel  = lblInterpret,
                                                  resButton = btnInterpret,
                                                  resValue  = txtValue,
                                                  res4Dots  = lbl4Dots,
                                                  resType   = txtType,
                                                  resErrors = varErrors},
                              guiCode       = txtCode,
                              guiWin        = win,
                              guiChrVar     = chv,
                              guiValFiller  = vfv,
                              guiChrFiller  = chfv,
                              guiStatus     = status} =
    do
        sel <- textCtrlGetStringSelection txtCode
        let runner = case sel of
                        "" -> tryIn
                        sl -> runTxtHPSelection sl
        refreshExpr model guiCtx
        liftTraceIO "running..."
        set status [text := "interpreting..."]
        set txtValue [enabled := False,
                      bgcolor := lightgrey]
        res <- runner model HP.interpret
        liftTraceIO "ready"
        case res of
            Left err ->
                warningDialog win "Error" err
            Right interp ->
                if HP.isIntType interp
                    then do
                        set status [text := ""]
                        set txtValue [enabled := True,
                                      bgcolor := white,
                                      text := HP.intKind interp]
                        set lbl4Dots [visible := False]
                        set txtType [visible := False]
                        set lblInterpret [text := "Kind:"]
                    else do
                        set lbl4Dots [visible := True]
                        set txtType [visible := True, text := HP.intType interp]
                        set lblInterpret [text := "Value:"]
                        -- now we fill the textbox --
                        poc <- liftIO $ get btnInterpret $ on command
                        let revert = do
                                        debugIO "Cancelling..."
                                        _ <- tryTakeMVar chv --NOTE: empty the var
                                        debugIO "Killing the char filler..."
                                        newchf <- spawn $ charFiller guiCtx
                                        swapMVar chfv newchf >>= kill
                                        debugIO "Killing the value filler..."
                                        newvf <- spawn $ valueFiller guiCtx
                                        swapMVar vfv newvf >>= kill
                                        debugIO "Ready"
                                        errs <- varGet varErrors
                                        set status [text := case errs of
                                                    [] -> ""
                                                    _  -> "Expression interpreted with errors: Check them by right-clicking on each one"]
                                        set txtValue [enabled := True,
                                                      bgcolor := case errs of
                                                                     [] -> white
                                                                     _  -> yellow]
                                        set btnInterpret [on command := poc, text := "Interpret"]
                         in liftIO $ set btnInterpret [text := "Cancel",
                                                       on command := revert]
                        liftDebugIO "sending the value to the Value Filler..."
                        readMVar vfv >>= flip sendTo ((HP.intValue interp), poc)

runTxtHPSelection :: String ->  HPS.ServerHandle ->
                     HP.HPage (Either HP.InterpreterError HP.Interpretation) -> IO (Either ErrorString HP.Interpretation)
runTxtHPSelection s model hpacc =
    do
        piRes <- tryIn' model HP.getPageIndex
        added <- tryIn' model $ HP.addPage
        case added of
                Left err ->
                    return $ Left err
                Right () ->
                    do
                        let cpi = case piRes of
                                        Left _err -> 0
                                        Right cp -> cp
                            newacc = HP.setPageText s (length s) >> hpacc
                        res <- tryIn model newacc
                        Right () <- tryIn' model $ HP.closePage >> HP.setPageIndex cpi 
                        return res

refreshExpr :: HPS.ServerHandle -> GUIContext -> IO ()
refreshExpr model GUICtx{guiCode = txtCode,
                         guiWin = win,
                         guiTimer = refreshTimer} =
   do
        set txtCode [font := fontFixed] -- Just to be sure
        txt <- get txtCode text
        ip <- textCtrlGetInsertionPoint txtCode
        
        res <- tryIn' model $ HP.setPageText txt ip
        
        case res of
            Left err ->
                warningDialog win "Error" err
            Right _ ->
                debugIO "refreshExpr done"
        
        timerStop refreshTimer


-- TIMER HANDLERS --------------------------------------------------------------
restartTimer _model GUICtx{guiTimer = refreshTimer} =
    do
        started <- timerStart refreshTimer 1000 True
        if started
            then return ()
            else fail "Could not start more timers"

-- INTERNAL UTILS --------------------------------------------------------------
type ErrorString = String

tryIn' :: HPS.ServerHandle -> HP.HPage x -> IO (Either ErrorString x)
tryIn' model hpacc = tryIn model $ hpacc >>= return . Right

tryIn :: HPS.ServerHandle -> HP.HPage (Either HP.InterpreterError x) -> IO (Either ErrorString x)
tryIn model hpacc =
    do
        debugIO "Trying..."
        res <- HPS.runIn model $ catchError (hpacc >>= return . Right)
                                            (\ioerr -> return $ Left ioerr)
        case res of
            Left err          -> return . Left  $ ioeGetErrorString err
            Right (Left err)  -> return . Left  $ HP.prettyPrintError err
            Right (Right val) -> return . Right $ val

-- FIND/REPLACE UTILS ----------------------------------------------------------
data FRFlags = FRFlags {frfGoingDown :: Bool,
                        frfMatchCase :: Bool,
                        frfWholeWord :: Bool,
                        frfWrapSearch :: Bool}
    deriving (Eq, Show)

buildFRFlags :: Bool -> Int -> IO FRFlags
buildFRFlags w x = return FRFlags {frfGoingDown = (x .&. wxFR_DOWN) /= 0,
                                   frfMatchCase = (x .&. wxFR_MATCHCASE) /= 0,
                                   frfWholeWord = (x .&. wxFR_WHOLEWORD) /= 0,
                                   frfWrapSearch = w}

openFindDialog :: HPS.ServerHandle -> GUIContext -> String -> Int -> IO ()
openFindDialog model guiCtx@GUICtx{guiWin = win,
                                   guiSearch = search} title dlgStyle =
    do
        frdialog <- findReplaceDialogCreate win search title $ dlgStyle + wxFR_NOWHOLEWORD
        let winSet k f = let hnd _ = f model guiCtx >> propagateEvent
                          in windowOnEvent frdialog [k] hnd hnd
        winSet wxEVT_COMMAND_FIND findNextButton
        winSet wxEVT_COMMAND_FIND_NEXT findNextButton
        winSet wxEVT_COMMAND_FIND_REPLACE findReplaceButton
        winSet wxEVT_COMMAND_FIND_REPLACE_ALL findReplaceAllButton
        set frdialog [visible := True]

findNextButton, findReplaceButton, findReplaceAllButton :: HPS.ServerHandle -> GUIContext -> IO ()
findNextButton model guiCtx@GUICtx{guiCode = txtCode,
                                   guiWin = win,
                                   guiSearch = search} =
    do
        s <- findReplaceDataGetFindString search
        fs <- findReplaceDataGetFlags search >>= buildFRFlags True
        mip <- findMatch s fs txtCode
        debugIO ("find/next", s, fs, mip)
        case mip of
            Nothing ->
                infoDialog win "Find Results" $ s ++ " not found."
            Just ip ->
                do
                    textCtrlSetInsertionPoint txtCode ip
                    refreshExpr model guiCtx 
                    textCtrlSetSelection txtCode ip (length s + ip)

findReplaceButton model guiCtx@GUICtx{guiCode = txtCode,
                                      guiWin = win,
                                      guiSearch = search} =
    do
        s <- findReplaceDataGetFindString search
        r <- findReplaceDataGetReplaceString search
        fs <- findReplaceDataGetFlags search >>= buildFRFlags True
        mip <- findMatch s fs txtCode
        debugIO ("replace", s, r, fs, mip)
        case mip of
            Nothing ->
                infoDialog win "Find Results" $ s ++ " not found."
            Just ip ->
                do
                    textCtrlReplace txtCode ip (length s + ip) r
                    textCtrlSetInsertionPoint txtCode ip
                    refreshExpr model guiCtx 
                    textCtrlSetSelection txtCode ip (length r + ip)
        
findReplaceAllButton _model GUICtx{guiCode = txtCode,
                                   guiSearch = search} =
    do
        s <- findReplaceDataGetFindString search
        r <- findReplaceDataGetReplaceString search        
        fs <- findReplaceDataGetFlags search >>= buildFRFlags False
        debugIO ("all", s, r, fs)
        textCtrlSetInsertionPoint txtCode 0
        unfoldM_ $ do
                        mip <- findMatch s fs txtCode
                        case mip of
                            Nothing ->
                                return mip
                            Just ip ->
                                do
                                    textCtrlReplace txtCode ip (length s + ip) r
                                    textCtrlSetInsertionPoint txtCode $ length r + ip
                                    return mip
        
findMatch :: String -> FRFlags -> TextCtrl () -> IO (Maybe Int)
findMatch query flags txtCode =
    do
        txt <- get txtCode text
        ip <- textCtrlGetInsertionPoint txtCode
        let (substring, string) = if frfMatchCase flags
                                    then (query, txt)
                                    else (map toLower query, map toLower txt)
            funct = if frfGoingDown flags
                        then nextMatch (ip + 1)
                        else prevMatch ip
            (mip, wrapped) = funct substring string
        return $ if (not $ frfWrapSearch flags) && wrapped
                    then Nothing
                    else mip
        

prevMatch, nextMatch :: Int -> String -> String -> (Maybe Int, Bool)
prevMatch _ [] _ = (Nothing, True) -- When looking for nothing, that's what you get
prevMatch from substring string | length string < from || from <= 0 = prevMatch (length string) substring string
                                | otherwise =
                                        case nextMatch (fromBack from) (reverse substring) (reverse string) of
                                            (Nothing, wrapped) -> (Nothing, wrapped)
                                            (Just ri, wrapped) -> (Just $ fromBack (ri + length substring), wrapped)
    where fromBack x = length string - x

nextMatch _ [] _ = (Nothing, True) -- When looking for nothing, that's what you get
nextMatch from substring string | length substring > length string = (Nothing, True)
                                | length string <= from = nextMatch 0 substring string
                                | otherwise =
                                        let after = drop from string
                                            before = take (from + length substring) string
                                            aIndex = indexOf substring after
                                            bIndex = indexOf substring before
                                         in case aIndex of
                                                Just ai ->
                                                    (Just $ from + ai,  False)
                                                Nothing ->
                                                    case bIndex of
                                                        Nothing -> (Nothing, True)
                                                        Just bi -> (Just bi, True)
    
indexOf :: String -> String -> Maybe Int
indexOf substring string = findIndex (isPrefixOf substring) $ tails string

prettyShow :: PackageIdentifier -> String
prettyShow PackageIdentifier{pkgName = PackageName pkgname,
                             pkgVersion = pkgvsn} = pkgname ++ "-" ++ showVersion pkgvsn
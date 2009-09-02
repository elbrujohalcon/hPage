{-# LANGUAGE GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             FunctionalDependencies,
             UndecidableInstances #-}
             
module HPage.GUI.FreeTextWindow ( gui ) where

import Control.Concurrent.Process
import Control.Concurrent.MVar
import System.FilePath
import System.IO.Error hiding (try)
import Data.List
import Control.Monad.Error
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.Dialogs
import Graphics.UI.WXCore.Events
import qualified HPage.Control as HP
import qualified HPage.Server as HPS
import Utils.Log

data GUIResultRow = GUIRRow { grrButton :: Button (),
                              grrText   :: TextCtrl ()}

data GUIResults = GUIRes { resValue :: GUIResultRow,
                           resType  :: GUIResultRow,
                           resKind  :: GUIResultRow }

data GUIContext  = GUICtx { guiWin :: Frame (),
                            guiPages :: SingleListBox (),
                            guiModules :: SingleListBox (),
                            guiCode :: TextCtrl (),
                            guiResults :: GUIResults,
                            guiStatus :: StatusField,
                            guiTimer :: Var (TimerEx ()) } 

gui :: IO ()
gui =
    do
        -- Server context
        model <- HPS.start
        
        win <- frame [text := "hPage"]
        topLevelWindowSetIconFromFile win "../res/images/icon/hpage.tif"
        
        set win [on closing := HPS.stop model >> propagateEvent]
        
        -- Containers
        pnl <- panel win []
        splLR <- splitterWindow pnl []
        pnlR <- panel splLR []
        
        -- Text page...
    --  txtCode <- styledTextCtrl win []
        txtCode <- textCtrlRich splLR [font := fontFixed{_fontSize = 12}]
        
        -- Document Selector
        lstModules <- singleListBox pnlR [style := wxLB_NEEDED_SB]
        lstPages <- singleListBox pnlR [style := wxLB_NEEDED_SB]

        -- Results list
        txtValue <- textCtrlRich pnlR [style := wxTE_READONLY]
        txtType <- textCtrlRich pnlR [style := wxTE_READONLY]
        txtKind <- textCtrlRich pnlR [style := wxTE_READONLY]
        
        -- Status bar...
        status <- statusField [text := "hello... this is hPage! type in your instructions :)"]
        refreshTimer <- timer win [interval := 1000000, on command := debugIO "Inactivity detected"]
        varTimer <- varCreate refreshTimer
        set win [statusBar := [status]]
        
        btnGetValue <- button pnlR [text := "Value"]
        btnGetType <- button pnlR [text := "Type"]
        btnGetKind <- button pnlR [text := "Kind"]
        
        let grrValue = GUIRRow btnGetValue txtValue
        let grrType = GUIRRow btnGetType txtType
        let grrKind = GUIRRow btnGetKind txtKind
        let guiRes = GUIRes grrValue grrType grrKind
        let guiCtx = GUICtx win lstPages lstModules txtCode guiRes status varTimer
        let onCmd name acc = traceIO ("onCmd", name) >> acc model guiCtx

        set btnGetValue [on command := onCmd "getValue" getValue]
        set btnGetType [on command := onCmd "getType" getType]
        set btnGetKind [on command := onCmd "getKind" getKind]
        
        -- Events
        set lstPages [on select := onCmd "pageChange" pageChange]
        set txtCode [on keyboard := \_ -> onCmd "restartTimer" restartTimer >> propagateEvent,
                     on mouse :=  \e -> case e of
                                            MouseLeftUp _ _ -> onCmd "mouseEvent" restartTimer >> propagateEvent
                                            MouseLeftDClick _ _ -> onCmd "mouseEvent" restartTimer >> propagateEvent
                                            _ -> propagateEvent]
        
        -- Menu bar...
        -- menuBar win []
        mnuPage <- menuPane [text := "Page"]
        mitNew  <- menuItem mnuPage [text := "&New\tCtrl-n",    on command := onCmd "runHP' addPage" $ runHP' HP.addPage]
        menuItem mnuPage [text := "&Close\tCtrl-w",             on command := onCmd "runHP' closePage" $ runHP' HP.closePage]
        menuItem mnuPage [text := "&Close All\tCtrl-Shift-w",   on command := onCmd "runHP' closeAllPages" $ runHP' HP.closeAllPages]
        menuLine mnuPage
        mitOpen <- menuItem mnuPage [text := "&Open...\tCtrl-o", on command := onCmd "openPage" openPage]
        mitSave <- menuItem mnuPage [text := "&Save\tCtrl-s",    on command := onCmd "savePage" savePage]
        menuItem mnuPage [text := "&Save as...\tCtrl-Shift-s",   on command := onCmd "savePageAs" savePageAs]
        menuLine mnuPage
        menuQuit mnuPage []
        
        mnuEdit <- menuPane [text := "Edit"]
        menuItem mnuEdit [text := "&Undo\tCtrl-z",         on command := onCmd "runHP' undo" $ runHP' HP.undo]
        menuItem mnuEdit [text := "&Redo\tCtrl-Shift-z",   on command := onCmd "runHP' redo" $ runHP' HP.redo]
        menuLine mnuEdit
        mitCut  <- menuItem mnuEdit [text := "C&ut\tCtrl-x",        on command := onCmd "cut" cut]
        mitCopy <- menuItem mnuEdit [text := "&Copy\tCtrl-c",       on command := onCmd "copy" copy]
        mitPaste <- menuItem mnuEdit [text := "&Paste\tCtrl-v",     on command := onCmd "paste" paste]
        menuLine mnuEdit
        menuItem mnuEdit [text := "&Find...\tCtrl-f",               on command := onCmd "find" $ \_ _ -> return ()]
        menuItem mnuEdit [text := "&Find Next\tCtrl-g",             on command := onCmd "findNext" $ \_ _ -> return ()]

        mnuHask <- menuPane [text := "Haskell"]
        menuItem mnuHask [text := "&Load module...\tCtrl-l",        on command := onCmd "loadModule" loadModule]
        menuItem mnuHask [text := "&Load module by name...\tCtrl-Shift-l",
                                                                    on command := onCmd "loadModuleByName" loadModuleByName]
        mitReload <- menuItem mnuHask [text := "&Reload\tCtrl-r",   on command := onCmd "reloadModules" reloadModules]
        menuLine mnuHask
        menuItem mnuHask [text := "&Value of Expression\tCtrl-e",   on command := onCmd "getValue" getValue]
        menuItem mnuHask [text := "&Type of Expression\tCtrl-t",    on command := onCmd "getType" getType]
        menuItem mnuHask [text := "&Kind of Expression\tCtrl-k",    on command := onCmd "getKind" getKind]
        
        mnuHelp <- menuHelp []
        menuAbout mnuHelp [on command := infoDialog win "About hPage" "Author: Fernando Brujo Benavides"]
        
        set win [menuBar := [mnuPage, mnuEdit, mnuHask, mnuHelp]]
    
        -- Tool bar...
        tbMain <- toolBarEx win True True []
        toolMenu tbMain mitNew "New"  "../res/images/new.png" [tooltip := "New"]
        toolMenu tbMain mitOpen "Open" "../res/images/open.png" [tooltip := "Open"]
        toolMenu tbMain mitSave "Save" "../res/images/save.png" [tooltip := "Save"]
        toolMenu tbMain mitCut "Cut"  "../res/images/cut.png" [tooltip := "Cut"]
        toolMenu tbMain mitCopy "Copy" "../res/images/copy.png" [tooltip := "Copy"]
        toolMenu tbMain mitPaste "Paste" "../res/images/paste.png" [tooltip := "Paste"]
        toolMenu tbMain mitReload "Reload" "../res/images/reload.png" [tooltip := "Reload Modules"]
        
        -- Layout settings
        let txtCodeL    = fill $ widget txtCode
            lstPagesL   = fill $ boxed "Pages" $ fill $ widget lstPages
            lstModulesL = fill $ boxed "Modules" $ fill $ widget lstModules
            valueRowL   = [widget btnGetValue, hfill $ widget txtValue]
            typeRowL    = [widget btnGetType, hfill $ widget txtType]
            kindRowL    = [widget btnGetKind, hfill $ widget txtKind]
            resultsGridL= hfill $ boxed "Expression" $ grid 5 0 [valueRowL, typeRowL, kindRowL]
            leftL       = container pnlR $ column 5 [resultsGridL, lstPagesL, lstModulesL]
        set win [layout := container pnl $ fill $ vsplit splLR 7 400 leftL txtCodeL,
                 clientSize := sz 800 600]

        -- ...and RUN!
        refreshPage model guiCtx
        focusOn txtCode

-- EVENT HANDLERS --------------------------------------------------------------
refreshPage, savePageAs, savePage, openPage,
    pageChange, copy, cut, paste,
    restartTimer, killTimer,
    getValue, getType, getKind,
    loadModule, loadModuleByName, reloadModules :: HPS.ServerHandle -> GUIContext -> IO ()

getValue model guiCtx@GUICtx{guiResults = GUIRes{resValue = grrValue}} =
    runTxtHP HP.valueOf' model guiCtx grrValue

getType model guiCtx@GUICtx{guiResults = GUIRes{resType = grrType}} =
    runTxtHP HP.typeOf' model guiCtx grrType

getKind model guiCtx@GUICtx{guiResults = GUIRes{resKind = grrKind}} =
    runTxtHP HP.kindOf' model guiCtx grrKind

pageChange model guiCtx@GUICtx{guiPages = lstPages} =
    do
        i <- get lstPages selection
        case i of
            (-1) -> return ()
            _ -> runHP' (HP.setPageIndex i) model guiCtx

openPage model guiCtx@GUICtx{guiWin = win,
                             guiStatus = status} =
    do
        fileName <- fileOpenDialog win True True "Open file..." [("Haskells",["*.hs"]),
                                                                 ("Any file",["*.*"])] "" ""
        case fileName of
            Nothing ->
                return ()
            Just f ->
                do
                    set status [text := "opening..."]
                    runHP' (HP.openPage f) model guiCtx

savePageAs model guiCtx@GUICtx{guiWin = win, guiStatus = status} =
    do
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

cut model guiCtx@GUICtx{guiCode = txtCode} = textCtrlCut txtCode >> refreshPage model guiCtx

paste model guiCtx@GUICtx{guiCode = txtCode} = textCtrlPaste txtCode >> refreshPage model guiCtx

reloadModules = runHP HP.reloadModules

loadModule model guiCtx@GUICtx{guiWin = win, guiStatus = status} =
    do
        fileName <- fileOpenDialog win True True "Load Module..." [("Haskell Modules",["*.hs"])] "" ""
        case fileName of
            Nothing ->
                return ()
            Just f ->
                do
                    set status [text := "loading..."]
                    runHP (HP.loadModule f) model guiCtx

loadModuleByName model guiCtx@GUICtx{guiWin = win, guiStatus = status} =
    do
        moduleName <- textDialog win "Enter the module name" "Load Module..." ""
        case moduleName of
            "" ->
                return ()
            mn ->
                do
                    set status [text := "loading..."]
                    runHP (HP.loadModule mn) model guiCtx

refreshPage model guiCtx@GUICtx{guiWin = win,
                                guiPages = lstPages,
                                guiModules = lstModules,
                                guiCode = txtCode,
                                guiStatus = status} =
    do
        res <- tryIn' model $ do
                                pc <- HP.getPageCount
                                pages <- mapM HP.getPageNthDesc [0..pc-1]
                                ind <- HP.getPageIndex
                                txt <- HP.getPageText
                                lmsRes <- HP.getLoadedModules
                                let lms = case lmsRes of
                                            Left  _ -> []
                                            Right x -> x
                                return (lms, pages, ind, txt)
        case res of
            Left err ->
                warningDialog win "Error" err
            Right (ms, ps, i, t) ->
                do
                    -- Refresh the pages list
                    itemsDelete lstPages
                    (flip mapM) ps $ \pd ->
                                        let prefix = if HP.pIsModified pd
                                                        then "*"
                                                        else ""
                                            name   = case HP.pPath pd of
                                                         Nothing -> "new page"
                                                         Just fn -> takeFileName $ dropExtension fn
                                         in itemAppend lstPages $ prefix ++ name
                    set lstPages [selection := i]
                    -- Refresh the modules list
                    itemsDelete lstModules
                    (flip mapM) ms $ itemAppend lstModules
                    -- Refresh the current text
                    set txtCode [text := t]
                    set status [text := ""]
                    -- Refresh the current expression box
                    refreshExpr model guiCtx True

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

runTxtHP :: HP.HPage (MVar (Either HP.InterpreterError String)) -> 
            HPS.ServerHandle -> GUIContext -> GUIResultRow -> IO ()
runTxtHP hpacc model guiCtx@GUICtx{guiWin = win,
                                   guiStatus = status}
                            GUIRRow{grrButton = btn,
                                    grrText = txtBox} =
    do
        refreshExpr model guiCtx False
        res <- tryIn' model hpacc
        case res of
            Left err -> warningDialog win "Error" err
            Right var -> do
                            cancelled <- varCreate False
                            prevOnCmd <- get btn $ on command
                            prevText <- get btn text
                            let prevAttrs = [text := prevText,
                                             on command := prevOnCmd]
                            set btn [text := "Cancel",
                                     on command := cancelHP model cancelled]
                            set txtBox [enabled := False]
                            set status [text := "processing..."]
                            spawn . liftIO $ do
                                                val <- readMVar var
                                                wasCancelled <- varGet cancelled
                                                if wasCancelled
                                                    then
                                                        set status [text := "cancelled"]
                                                    else
                                                        do
                                                            set status [text := "ready"]
                                                            case val of
                                                                Left err -> warningDialog win "Error" $ HP.prettyPrintError err
                                                                Right txt -> set txtBox [text := txt]
                                                set txtBox [enabled := True]
                                                set btn prevAttrs
                            return ()

cancelHP :: HPS.ServerHandle -> Var Bool -> IO ()
cancelHP model cancelled = varSet cancelled True >> tryIn' model HP.cancel >> return ()        

refreshExpr :: HPS.ServerHandle -> GUIContext -> Bool -> IO ()
refreshExpr model guiCtx@GUICtx{guiResults = GUIRes{resValue = grrValue,
                                                    resType = grrType,
                                                    resKind = grrKind},
                                guiCode = txtCode,
                                guiWin = win} forceClear =
   do
        txt <- get txtCode text
        ip <- textCtrlGetInsertionPoint txtCode
        
        res <- tryIn' model $ HP.setPageText txt ip
        
        case res of
            Left err ->
                warningDialog win "Error" err
            Right changed ->
                if changed || forceClear
                    then mapM_ (flip set [text := ""] . grrText) [grrValue, grrType, grrKind]
                    else debugIO "dummy refreshExpr"

        killTimer model guiCtx


-- TIMER HANDLERS --------------------------------------------------------------
restartTimer model guiCtx@GUICtx{guiWin = win, guiTimer = varTimer} =
    do
        newRefreshTimer <- timer win [interval := 1000,
                                      on command := refreshExpr model guiCtx False]
        refreshTimer <- varSwap varTimer newRefreshTimer
        timerOnCommand refreshTimer $ return ()

killTimer _model GUICtx{guiWin = win, guiTimer = varTimer} =
    do
        -- kill the timer till there's new notices
        newRefreshTimer <- timer win [interval := 1000000, on command := debugIO "Inactivity detected"]
        refreshTimer <- varSwap varTimer newRefreshTimer
        timerOnCommand refreshTimer $ return ()

-- INTERNAL UTILS --------------------------------------------------------------
type ErrorString = String

tryIn' :: HPS.ServerHandle -> HP.HPage x -> IO (Either ErrorString x)
tryIn' model hpacc = tryIn model $ hpacc >>= return . Right

tryIn :: HPS.ServerHandle -> HP.HPage (Either HP.InterpreterError x) -> IO (Either ErrorString x)
tryIn model hpacc =
    do
        res <- HPS.runIn model $ catchError (hpacc >>= return . Right)
                                            (\ioerr -> return $ Left ioerr)
        case res of
            Left err          -> do
                                    errorIO err
                                    return . Left  $ ioeGetErrorString err
            Right (Left err)  -> return . Left  $ HP.prettyPrintError err
            Right (Right val) -> return . Right $ val
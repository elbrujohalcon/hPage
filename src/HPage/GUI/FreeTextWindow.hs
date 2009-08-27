{-# LANGUAGE GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             FunctionalDependencies,
             UndecidableInstances #-}
             
module HPage.GUI.FreeTextWindow ( gui ) where

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

data GUIResults t = GUIRes { resValue :: TextCtrl t,
                             resType  :: TextCtrl t,
                             resKind  :: TextCtrl t }

data GUIContext w l t r s  = GUICtx { guiWin :: Window w,
                                      guiPages :: SingleListBox l,
                                      guiModules :: SingleListBox l,
                                      guiCode :: TextCtrl t,
                                      guiResults :: GUIResults r,
                                      guiStatus :: StatusField,
                                      guiTimer :: Var (TimerEx ()) } 

gui :: IO ()
gui =
    do
        -- Server context
        model <- HPS.start
        
        win <- frame [text := "hPage"]
        topLevelWindowSetIconFromFile win "../res/images/icon/hpage.tif"
        
        --HACK: closing with an exception avoids wxWidgets ugly warnings on OSX
        set win [on closing := HPS.stop model >> undefined]
        
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
        txtValue <- textCtrlRich pnlR []
        txtType <- textCtrlRich pnlR []
        txtKind <- textCtrlRich pnlR []
        
        -- Status bar...
        status <- statusField [text := "hello... this is hPage! type in your instructions :)"]
        refreshTimer <- timer win [interval := 1000000, on command := debugIO "Inactivity detected"]
        varTimer <- varCreate refreshTimer
        set win [statusBar := [status]]
        
        let guiRes = GUIRes txtValue txtType txtKind
        let guiCtx = GUICtx win lstPages lstModules txtCode guiRes status varTimer
        let onCmd acc = acc model guiCtx
        
        btnGetValue <- button pnlR [text := "Value",on command := onCmd getValue]
        btnGetType <- button pnlR [text := "Type", on command := onCmd getType]
        btnGetKind <- button pnlR [text := "Kind", on command := onCmd getKind]
        
        -- Events
        set lstPages [on select := onCmd pageChange]
        set txtCode [on keyboard := (\_ -> onCmd restartTimer >> propagateEvent)]
        
        -- Menu bar...
        -- menuBar win []
        mnuPage <- menuPane [text := "Page"]
        mitNew  <- menuItem mnuPage [text := "&New\tCtrl-n",    on command := onCmd $ runHP' HP.addPage]
        menuItem mnuPage [text := "&Close\tCtrl-w",             on command := onCmd $ runHP' HP.closePage]
        menuItem mnuPage [text := "&Close All\tCtrl-Shift-w",   on command := onCmd $ runHP' HP.closeAllPages]
        menuLine mnuPage
        mitOpen <- menuItem mnuPage [text := "&Open...\tCtrl-o", on command := onCmd openPage]
        mitSave <- menuItem mnuPage [text := "&Save\tCtrl-s",    on command := onCmd savePage]
        menuItem mnuPage [text := "&Save as...\tCtrl-Shift-s",   on command := onCmd savePageAs]
        menuLine mnuPage
        menuQuit mnuPage []
        
        mnuEdit <- menuPane [text := "Edit"]
        menuItem mnuEdit [text := "&Undo\tCtrl-z",         on command := onCmd $ runHP' HP.undo]
        menuItem mnuEdit [text := "&Redo\tCtrl-Shift-z",   on command := onCmd $ runHP' HP.redo]
        menuLine mnuEdit
        mitCut  <- menuItem mnuEdit [text := "C&ut\tCtrl-x",        on command := onCmd cut]
        mitCopy <- menuItem mnuEdit [text := "&Copy\tCtrl-c",       on command := onCmd copy]
        mitPaste <- menuItem mnuEdit [text := "&Paste\tCtrl-v",     on command := onCmd paste]
        menuLine mnuEdit
        menuItem mnuEdit [text := "&Find...\tCtrl-f"]
        menuItem mnuEdit [text := "&Find Next\tCtrl-g"]

        mnuHask <- menuPane [text := "Haskell"]
        menuItem mnuHask [text := "&Load module...\tCtrl-l",        on command := onCmd loadModule]
        mitReload <- menuItem mnuHask [text := "&Reload\tCtrl-r",   on command := onCmd reloadModules]
        menuLine mnuHask
        menuItem mnuHask [text := "&Value of Expression\tCtrl-e",   on command := onCmd getValue]
        menuItem mnuHask [text := "&Type of Expression\tCtrl-t",    on command := onCmd getType]
        menuItem mnuHask [text := "&Kind of Expression\tCtrl-k",    on command := onCmd getKind]
        
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
            leftL       = container pnlR $ column 5 [lstPagesL, resultsGridL, lstModulesL]
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
    loadModule, reloadModules :: HPS.ServerHandle -> GUIContext w l t r s -> IO ()

getValue model GUICtx{guiWin = win, guiResults = GUIRes{resValue = txtValue}} =
    runTxtHP HP.valueOf model win txtValue

getType model GUICtx{guiWin = win, guiResults = GUIRes{resType = txtType}} =
    runTxtHP HP.typeOf model win txtType 

getKind model GUICtx{guiWin = win, guiResults = GUIRes{resKind = txtKind}} =
    runTxtHP HP.kindOf model win txtKind

pageChange model guiCtx@GUICtx{guiPages = lstPages} =
    do
        i <- get lstPages selection
        runHP' (HP.setPageIndex i) model guiCtx

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

savePage model guiCtx =
    do
        path <- HPS.runIn model $ HP.getPagePath
        case path of
            Nothing ->
                savePageAs model guiCtx
            _ ->
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

refreshPage model guiCtx@GUICtx{guiPages = lstPages,
                                guiModules = lstModules,
                                guiCode = txtCode,
                                guiStatus = status} =
    do
        (ms, ps, i, t) <- HPS.runIn model $ do
                                                pc <- HP.getPageCount
                                                pages <- mapM HP.getPageNthDesc [0..pc-1]
                                                ind <- HP.getPageIndex
                                                txt <- HP.getPageText
                                                lmsRes <- HP.getLoadedModules
                                                let lms = case lmsRes of
                                                            Left  _ -> []
                                                            Right x -> x
                                                return (lms, pages, ind, txt)
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

runHP' ::  HP.HPage () -> HPS.ServerHandle -> GUIContext w l t r s -> IO ()
runHP' a = runHP (a >>= return . Right)

runHP ::  HP.HPage (Either HP.InterpreterError ()) -> HPS.ServerHandle -> GUIContext w l t r s -> IO ()
runHP hpacc model guiCtx@GUICtx{guiWin = win} =
    do
        res <- HPS.runIn model hpacc
        case res of
            Left err ->
                errorDialog win "Error" $ HP.prettyPrintError err
            Right () ->
                refreshPage model guiCtx

runTxtHP :: HP.HPage (Either HP.InterpreterError String) -> 
            HPS.ServerHandle -> Window w -> TextCtrl t -> IO ()
runTxtHP hpacc model win txt =
    do
        res <- tryIn model hpacc
        case res of
            Left err -> warningDialog win "Error" err
            Right val -> set txt [text := val]

refreshExpr :: HPS.ServerHandle -> GUIContext w l t r s -> Bool -> IO ()
refreshExpr model guiCtx@GUICtx{guiResults = GUIRes{resValue = txtValue,
                                                    resType = txtType,
                                                    resKind = txtKind},
                                guiCode = txtCode} forceClear =
   do
        txt <- get txtCode text
        prevTxt <- HPS.runIn model HP.getPageText
        prevIndex <- HPS.runIn model HP.getExprIndex

        let txtChanged = txt /= prevTxt
        if txtChanged
            then HPS.runIn model $ HP.setPageText txt
            else return ()
        
        --TODO: This *must* be done inside HPage (for example, on setPageText)
        ip <- textCtrlGetInsertionPoint txtCode
        let exprIndex = length $ filter ("" ==) $ lines $ take ip txt
        let exprChanged = exprIndex /= prevIndex
        if exprChanged
            then HPS.runIn model $ do
                                        ec <- HP.getExprCount
                                        if ec > exprIndex
                                            then HP.setExprIndex exprIndex
                                            else return ()
            else return ()
        
        debugIO ("Insertion point: ", ip,
                 "Expression Index: ", exprIndex,
                 "Forced:", forceClear,
                 "Text changed:", txtChanged,
                 "Expr changed:", exprChanged)
        
        if exprChanged || txtChanged || forceClear
            then mapM_ (flip set [text := ""]) [txtValue, txtType, txtKind]
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

tryIn :: HPS.ServerHandle -> HP.HPage (Either HP.InterpreterError x) -> IO (Either ErrorString x)
tryIn model hpacc =
    do
        res <- HPS.runIn model $ catchError (hpacc >>= return . Right)
                                            (\ioerr -> return $ Left ioerr)
        case res of
            Left err          -> return . Left  $ ioeGetErrorString err
            Right (Left err)  -> return . Left  $ HP.prettyPrintError err
            Right (Right val) -> return . Right $ val
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
import qualified HPage.Stub.Control as HP
import qualified HPage.Stub.Server as HPS

data GUIResults t = GUIRes { resName  :: TextCtrl t,
                             resValue :: TextCtrl t,
                             resType  :: TextCtrl t,
                             resKind  :: TextCtrl t }

data GUIContext w l t r s  = GUICtx { guiWin :: Window w,
                                      guiPages :: l,
                                      guiModules :: l,
                                      guiCode :: TextCtrl t,
                                      guiResults :: GUIResults r,
                                      guiStatus :: s } 

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
        txtName <- textEntry pnlR []
        txtValue <- textEntry pnlR []
        txtType <- textEntry pnlR []
        txtKind <- textEntry pnlR []
        
        -- Status bar...
        status <- statusField [text := "hello... this is hPage! type in your instructions :)"]
        set win [statusBar := [status]]
        
        let guiRes = GUIRes txtName txtValue txtType txtKind
        let guiCtx = GUICtx win lstPages lstModules txtCode guiRes status
        let onCmd acc = do
                            mustRefresh <- acc model guiCtx
                            if mustRefresh
                                then display model guiCtx
                                else return ()
        
        btnSetName <- button pnlR [text := "Set", on command := onCmd nameExpr]
        btnGetValue <- button pnlR [text := "Get",on command := onCmd getValue]
        btnGetType <- button pnlR [text := "Get", on command := onCmd getType]
        btnGetKind <- button pnlR [text := "Get", on command := onCmd getKind]
        
        -- Events
        set lstPages [on select := onCmd pageChange]
        controlOnText txtCode $ onCmd updatePage
        
        -- Menu bar...
        -- menuBar win []
        mnuPage <- menuPane [text := "Page"]
        mitNew  <- menuItem mnuPage [text := "&New\tCtrl-n",     on command := onCmd $ runHP HP.addPage]
        menuItem mnuPage [text := "&Close\tCtrl-w",   on command := onCmd $ runHP HP.closePage]
        menuItem mnuPage [text := "&Close All\tCtrl-Shift-w",   on command := onCmd $ runHP HP.closeAllPages]
        menuLine mnuPage
        mitOpen <- menuItem mnuPage [text := "&Open...\tCtrl-o", on command := onCmd openPage]
        mitSave <- menuItem mnuPage [text := "&Save\tCtrl-s",    on command := onCmd savePage]
        menuItem mnuPage [text := "&Save as...\tCtrl-Shift-s",   on command := onCmd savePageAs]
        menuLine mnuPage
        menuQuit mnuPage []
        
        mnuEdit <- menuPane [text := "Edit"]
        menuItem mnuEdit [text := "&Undo\tCtrl-z",         on command := onCmd $ runHP HP.undo]
        menuItem mnuEdit [text := "&Redo\tCtrl-Shift-z",   on command := onCmd $ runHP HP.redo]
        menuLine mnuEdit
        mitCut  <- menuItem mnuEdit [text := "C&ut\tCtrl-x",        on command := onCmd cut]
        mitCopy <- menuItem mnuEdit [text := "&Copy\tCtrl-c",       on command := onCmd copy]
        mitPaste <- menuItem mnuEdit [text := "&Paste\tCtrl-v",     on command := onCmd paste]
        menuLine mnuEdit
        menuItem mnuEdit [text := "&Find...\tCtrl-f"]
        menuItem mnuEdit [text := "&Find Next\tCtrl-g"]

        mnuHask <- menuPane [text := "Haskell"]
        menuItem mnuHask [text := "&Load module...\tCtrl-l",        on command := onCmd loadModule]
        mitReload <- menuItem mnuHask [text := "&Reload\tCtrl-r",   on command := onCmd $ runHP HP.reloadModules]
        menuLine mnuHask
        menuItem mnuHask [text := "&Value of Expression\tCtrl-e",   on command := onCmd getValue]
        menuItem mnuHask [text := "&Type of Expression\tCtrl-t",    on command := onCmd getType]
        menuItem mnuHask [text := "&Kind of Expression\tCtrl-k",    on command := onCmd getKind]
        menuLine mnuHask
        menuItem mnuHask [text := "&Name Expression\tAlt-n",        on command := onCmd nameExpr]
        menuItem mnuHask [text := "&Unname Expression\tAlt-u",      on command := onCmd unnameExpr]
        
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
            nameRowL    = [label "Name", hfill $ widget txtName, widget btnSetName]
            valueRowL   = [label "Value", hfill $ widget txtValue, widget btnGetValue]
            typeRowL    = [label "Type", hfill $ widget txtType, widget btnGetType]
            kindRowL    = [label "Kind", hfill $ widget txtKind, widget btnGetKind]
            resultsGridL= hfill $ boxed "Expression" $ grid 5 2 [nameRowL, valueRowL, typeRowL, kindRowL]
            leftL       = container pnlR $ column 5 [lstPagesL, resultsGridL, lstModulesL]
        set win [layout := container pnl $ fill $ vsplit splLR 7 400 leftL txtCodeL,
                 clientSize := sz 800 600]

        -- ...and RUN!
        display model guiCtx
        focusOn txtCode

display :: (Selection l, Items l [Char], Textual s) =>
                            HPS.ServerHandle -> GUIContext w l t r s -> IO ()
display model GUICtx{guiPages = lstPages,
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
        itemsDelete lstModules
        (flip mapM) ms $ itemAppend lstModules
        set txtCode [text := t]
        set status [text := ""]
        return ()

getValue, getType, getKind, nameExpr, unnameExpr :: HPS.ServerHandle -> GUIContext w l t r s -> IO Bool

getValue model GUICtx{guiWin = win, guiResults = GUIRes{resValue = txtValue}} =
    runTxtHP HP.valueOf model win txtValue >> return False 

getType model GUICtx{guiWin = win, guiResults = GUIRes{resType = txtType}} =
    runTxtHP HP.typeOf model win txtType >> return False 

getKind model GUICtx{guiWin = win, guiResults = GUIRes{resKind = txtKind}} =
    runTxtHP HP.kindOf model win txtKind >> return False

unnameExpr model guiCtx =
    do
        let txtName = resName $ guiResults guiCtx
        set txtName [text := ""]
        nameExpr model guiCtx

nameExpr model GUICtx{guiWin = win, guiResults = GUIRes{resName = txtName}} =
    do
        txt <- get txtName text
        let acc = case txt of
                    "" ->
                        HP.removeExprName
                    exprName ->
                        HP.setExprName exprName
        let hpacc = do
                        acc
                        newName <- HP.getExprName
                        case newName of
                            Nothing ->
                                return $ Right ""
                            Just nm ->
                                return $ Right nm 
        runTxtHP hpacc model win txtName
        return False

runTxtHP :: HP.HPage (Either HP.InterpreterError String) -> 
            HPS.ServerHandle -> Window w -> TextCtrl t -> IO ()
runTxtHP hpacc model win txt =
    do
        (worked, res) <- HPS.runIn model $ try hpacc
        let sRes = case res of
                        Left err -> HP.prettyPrintError err
                        Right val -> val
        if worked
            then txt `set` [text := sRes]
            else warningDialog win "Error" sRes
    where try a = (do
                        r <- a
                        return (True, r))
                    `catchError` (\err -> return (False, Right $ ioeGetErrorString err))

runHP ::  (Selection l, Items l [Char], Textual s) =>
                            HP.HPage x -> HPS.ServerHandle -> GUIContext w l t r s -> IO Bool
runHP hpacc model _ = HPS.runIn model hpacc >> return True

updatePage, savePageAs, savePage, openPage,
    pageChange, copy, cut, paste,
    loadModule, refreshExprs :: (Selection l, Items l [Char], Textual s) =>
                                    HPS.ServerHandle -> GUIContext w l t r s -> IO Bool
pageChange model guiCtx =
    do
        i <- get (guiPages guiCtx) selection
        HPS.runIn model $ HP.setPageIndex i
        refreshExprs model guiCtx
        return True

openPage model GUICtx{guiWin = win,
                      guiResults = GUIRes{resName = txtName,
                                          resValue = txtValue,
                                          resType = txtType,
                                          resKind = txtKind},
                      guiStatus = status} =
    do
        fileName <- fileOpenDialog win True True "Open file..." [("Haskells",["*.hs"]),
                                                                 ("Any file",["*.*"])] "" ""
        case fileName of
            Nothing ->
                return False
            Just f ->
                do
                    set status [text := "opening..."]
                    HPS.runIn model $ HP.openPage f
                    mapM (flip set [text := ""]) [txtName, txtValue, txtType, txtKind]
                    return True

savePageAs model GUICtx{guiWin = win, guiStatus = status} =
    do
        fileName <- fileSaveDialog win True True "Save file..." [("Haskells",["*.hs"]),
                                                                 ("Any file",["*.*"])] "" ""
        case fileName of
            Nothing ->
                return False
            Just f ->
                do
                    set status [text := "saving..."]
                    HPS.runIn model $ HP.savePageAs f
                    return True

savePage model guiCtx =
    do
        path <- HPS.runIn model $ HP.getPagePath
        case path of
            Nothing ->
                savePageAs model guiCtx
            _ ->
                do
                    set (guiStatus guiCtx) [text := "saving..."]
                    HPS.runIn model HP.savePage
                    return True

updatePage model guiCtx =
    do
        txt <- get (guiCode guiCtx) text
        HPS.runIn model $ HP.setPageText txt
        refreshExprs model guiCtx

copy _model GUICtx{guiCode = txtCode} =
    textCtrlCopy txtCode >> return False
cut model guiCtx =
    do
        textCtrlCut (guiCode guiCtx)
        updatePage model guiCtx

paste model guiCtx =
    do
        textCtrlPaste (guiCode guiCtx)
        updatePage model guiCtx

loadModule model GUICtx{guiWin = win, guiStatus = status} =
    do
        fileName <- fileOpenDialog win True True "Load Module..." [("Haskell Modules",["*.hs"])] "" ""
        case fileName of
            Nothing ->
                return False
            Just f ->
                do
                    set status [text := "loading..."]
                    res <- HPS.runIn model $ HP.loadModule f
                    case res of
                        Left err ->
                            do
                                errorDialog win "Error" $ HP.prettyPrintError err
                                return True
                        Right () -> return True

refreshExprs model GUICtx{guiResults = GUIRes{resName = txtName,
                                              resValue = txtValue,
                                              resType = txtType,
                                              resKind = txtKind}} =
    do
        nm <- HPS.runIn model HP.getExprName
        set txtName [text := case nm of
                                Nothing -> ""
                                Just n -> n]
        mapM (flip set [text := ""]) [txtValue, txtType, txtKind]
        return False
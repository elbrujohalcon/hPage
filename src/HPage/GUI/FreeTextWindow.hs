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

gui :: IO ()
gui =
    do
        -- Server context
        model <- HPS.start
        
        -- Clipboard
        clipboard <- clipboardCreate 
        
        win <- frame [text := "hPage"]
        topLevelWindowSetIconFromFile win "../res/images/icon/hpage.tif"
        
        --HACK: closing with an exception avoids wxWidgets ugly warnings on OSX
        set win [on closing := HPS.stop model >> undefined]
        
        -- Containers
        pnl <- panel win []
        splLR <- splitterWindow pnl []
        pnlRTB <- panel splLR []
        pnlLTB <- panel splLR []
        
        -- Text page...
    --  txtCode <- styledTextCtrl win []
        txtCode <- textCtrlRich pnlRTB [font := fontFixed{_fontSize = 12}]
        
        -- Document Selector
        lstModules <- singleListBox pnlLTB [style := wxLB_NEEDED_SB]
        lstPages <- singleListBox pnlLTB [style := wxLB_NEEDED_SB]

        -- Results list
        lstResults <- listCtrl pnlRTB [columns := [("Name", AlignLeft, 100),
                                                   ("Expression", AlignLeft, 100),
                                                   ("Value", AlignLeft, 100),
                                                   ("Type", AlignLeft, 100),
                                                   ("Kind", AlignLeft, 100)]]
        
        -- Status bar...
        status <- statusField [text := "hello... this is hPage! type in your instructions :)"]
        set win [statusBar := [status]]
        
        let onCmd acc = do
                            mustRefresh <- acc model win clipboard lstPages lstModules txtCode lstResults status
                            if mustRefresh
                                then display model win clipboard lstPages lstModules txtCode lstResults status
                                else return ()
        
        -- Events
        set lstPages [on select := onCmd pageChange]
        controlOnText txtCode $ onCmd updatePage
        listCtrlOnListEvent lstResults $ (\e -> onCmd $ onListEvent e) 
        
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
        menuItem mnuEdit [text := "&Find...\tCtrl-f",               on command := onCmd $ say "find"]
        menuItem mnuEdit [text := "&Find Next\tCtrl-g",             on command := onCmd $ say "findNext"]

        mnuHask <- menuPane [text := "Haskell"]
        menuItem mnuHask [text := "&Load module...\tCtrl-l",        on command := onCmd loadModule]
        menuItem mnuHask [text := "&Reload\tCtrl-r",   on command := onCmd $ runHP HP.reloadModules]
        menuLine mnuHask
        mitRefresh <- menuItem mnuHask [text := "&Refresh Grid\tCtrl-r", on command := onCmd refreshExprs]
        menuItem mnuHask [text := "&Value of Expression\tCtrl-e",   on command := onCmd $ runGridHP 2 HP.valueOf]
        menuItem mnuHask [text := "&Type of Expression\tCtrl-t",    on command := onCmd $ runGridHP 3 HP.typeOf]
        menuItem mnuHask [text := "&Kind of Expression\tCtrl-k",    on command := onCmd $ runGridHP 4 HP.kindOf]
        menuLine mnuHask
        menuItem mnuHask [text := "&Name Expression\tAlt-n",        on command := onCmd nameExpr]
        menuItem mnuHask [text := "&Unname Expression\tAlt-u",      on command := onCmd $ runGridHP 1 $ HP.removeExprName >> return (Right "")]
        
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
        toolMenu tbMain mitRefresh "Refresh" "../res/images/reload.png" [tooltip := "Refresh"]
        
        -- Layout settings
        let lstPagesL   = fill $ boxed "Pages" $ fill $ widget lstPages
            lstModulesL = fill $ boxed "Modules" $ fill $ widget lstModules
            leftL       = container pnlLTB $ column 5 [lstPagesL, lstModulesL]
            txtCodeL    = fill $ boxed "Code" $ fill $ widget txtCode
            lstResultsL = fill $ boxed "Expressions" $ fill $ widget lstResults
            rightL      = container pnlRTB $ column 5 [txtCodeL, lstResultsL]
        set win [layout := container pnl $ fill $ vsplit splLR 7 175 leftL rightL,
                 clientSize := sz 800 600]

        -- ...and RUN!
        display model win clipboard lstPages lstModules txtCode lstResults status
        focusOn txtCode
    where
        say x _model _win _clipboard _lstPages _lstModules _txtCode lstResults _status = itemAppend lstResults [x, "an expr", "a value", "a type", "a kind"] >> return False

display :: (Selection lst, Items lst [Char], Textual sb, Items lr [String]) =>
                            HPS.ServerHandle -> Window frame -> Clipboard a -> lst -> lst ->
                                TextCtrl txt -> lr -> sb -> IO ()
display model _win _clipboard lstPages lstModules txtCode _lstResults status =
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

runGridHP ::  (Selection lst, Items lst [Char], Textual sb, Items (ListCtrl lr) [String]) =>
                            Int -> HP.HPage (Either HP.InterpreterError String) -> HPS.ServerHandle -> Window frame ->
                                Clipboard a -> lst -> lst -> TextCtrl txt -> ListCtrl lr -> sb -> IO Bool
runGridHP col hpacc model win _ _ _ _ lstResults _ =
    do
        (worked, res) <- HPS.runIn model $ try hpacc
        let sRes = case res of
                        Left err -> HP.prettyPrintError err
                        Right val -> val
        if worked
            then do
                    r <- HPS.runIn model HP.getExprIndex
                    prevItem <- get lstResults $ item r
                    set lstResults [item r := (replaceAt col sRes prevItem)]
                    listCtrlSetItemState lstResults r wxLIST_STATE_SELECTED wxLIST_STATE_SELECTED
                    return ()
            else
                warningDialog win "Error" sRes 
        return False
    where replaceAt i x xs = let (before, (_:after)) = splitAt i xs
                              in before ++ (x:after)
          try a = (do
                        r <- a
                        return (True, r))
                    `catchError` (\err -> return (False, Right $ ioeGetErrorString err))

runHP ::  (Selection lst, Items lst [Char], Textual sb, Items lr [String]) =>
                            HP.HPage x -> HPS.ServerHandle -> Window frame -> Clipboard a -> lst -> lst ->
                                TextCtrl txt -> lr -> sb -> IO Bool
runHP hpacc model _ _ _ _ _ _ _ = HPS.runIn model hpacc >> return True

onListEvent :: (Selection lst, Items lst [Char], Textual sb, Items (ListCtrl lr) [String]) =>
                            EventList -> HPS.ServerHandle -> Window frame -> Clipboard a -> lst -> lst ->
                                TextCtrl txt -> ListCtrl lr -> sb -> IO Bool
onListEvent ev =
    case ev of
        ListItemSelected i ->
            runHP $ HP.setExprIndex i
        _ ->
            (\_ _ _ _ _ _ _ _ -> return False) --NOTE: we ignore the rest of the parameters

updatePage, savePageAs, savePage, openPage,
    pageChange, copy, cut, paste, nameExpr,
    loadModule, refreshExprs :: (Selection lst, Items lst [Char], Textual sb, Items (ListCtrl lr) [String]) =>
                                    HPS.ServerHandle -> Window frame -> Clipboard a -> lst -> lst ->
                                    TextCtrl txt -> ListCtrl lr -> sb -> IO Bool

nameExpr model win clipboard lstPages lstModules txtCode lstResults status =
    do
        dlgRes <- textDialog win "Choose name..." "hPage" ""
        case dlgRes of
            "" ->
                return False
            exprName ->
                let hpacc = do
                                HP.setExprName exprName
                                newName <- HP.getExprName
                                case newName of
                                    Nothing ->
                                        return $ Right ""
                                    Just nm ->
                                        return $ Right nm 
                 in runGridHP 0 hpacc model win clipboard lstPages lstModules txtCode lstResults status
                                

pageChange model win clipboard lstPages lstModules txtCode lstResults status =
    do
        i <- get lstPages selection
        HPS.runIn model $ HP.setPageIndex i
        refreshExprs model win clipboard lstPages lstModules txtCode lstResults status
        return True

openPage model win _ _ _ _ lstResults status =
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
                    itemsDelete lstResults
                    return True

savePageAs model win _ _ _ _ _ status =
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

savePage model win clipboard lstPages lstModules txtCode lstResults status =
    do
        path <- HPS.runIn model $ HP.getPagePath
        case path of
            Nothing ->
                savePageAs model win clipboard lstPages lstModules txtCode lstResults status
            _ ->
                do
                    set status [text := "saving..."]
                    HPS.runIn model HP.savePage
                    return True

updatePage model win clipboard lstPages lstModules txtCode lstResults status =
    do
        txt <- get txtCode text
        HPS.runIn model $ HP.setPageText txt
        refreshExprs model win clipboard lstPages lstModules txtCode lstResults status

copy _ _ _ _ _ txtCode _ _ =
    textCtrlCopy txtCode >> return False
cut model win clipboard lstPages lstModules txtCode lstResults status =
    do
        textCtrlCut txtCode
        updatePage model win clipboard lstPages lstModules txtCode lstResults status

paste model win clipboard lstPages lstModules txtCode lstResults status =
    do
        textCtrlPaste txtCode
        updatePage model win clipboard lstPages lstModules txtCode lstResults status

loadModule model win _ _ _ _ _ status =
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

refreshExprs model _ _ _ _ _ lstResults _ =
    do
        itemsDelete lstResults
        rs <- HPS.runIn model $ do
                                    ec <- HP.getExprCount
                                    (flip mapM) [0..ec-1] $ \i ->
                                                            do
                                                                en <- HP.getExprNthName i
                                                                ex <- HP.getExprNthText i
                                                                return (en, ex)
        forM_ rs $ \(n, e) ->
                    let n' = case n of
                                Nothing -> ""
                                Just nm -> nm
                     in itemAppend lstResults [n', e, "", "", ""]
        return False
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
import Data.Bits
import Data.Char (toLower)
import Control.Monad.Error
import Control.Monad.Loops
import Graphics.UI.WX
import Graphics.UI.WX.Dialogs.Extra
import Graphics.UI.WXCore
import Graphics.UI.WXCore.WxcDefs.ExtraIdentities
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.Dialogs
import Graphics.UI.WXCore.Events
import Graphics.UI.WXCore.WxcClasses
import qualified HPage.Control as HP
import qualified HPage.Server as HPS
import Utils.Log

import Paths_hpage -- cabal locations of data files

imageFile :: FilePath -> IO FilePath
imageFile = getDataFileName . ("res/images/"++)

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
                            guiTimer :: Var (TimerEx ()),
                            guiSearch :: FindReplaceData ()} 

gui :: IO ()
gui =
    do
        -- Server context
        model <- HPS.start
        
        win <- frame [text := "hPage"]
        imageFile "icon/hpage.tif" >>= topLevelWindowSetIconFromFile win 
        
        set win [on closing := HPS.stop model >> propagateEvent]

        -- Containers
        pnl <- panel win []
        splLR <- splitterWindow pnl []
        pnlL <- panel splLR []
        pnlR <- panel splLR []
        
        -- Text page...
    --  txtCode <- styledTextCtrl win []
        txtCode <- textCtrl pnlR [font := fontFixed, text := ""]
        
        -- Document Selector
        lstModules <- singleListBox pnlL [style := wxLB_NEEDED_SB]
        lstPages <- singleListBox pnlL [style := wxLB_NEEDED_SB]

        -- Results list
        txtValue <- textEntry pnlR [style := wxTE_READONLY]
        txtType <- textEntry pnlR [style := wxTE_READONLY]
        txtKind <- textEntry pnlR [style := wxTE_READONLY]
        
        -- Status bar...
        status <- statusField [text := "hello... this is hPage! type in your instructions :)"]
        refreshTimer <- timer win [interval := 1000000, on command := debugIO "Inactivity detected"]
        varTimer <- varCreate refreshTimer
        set win [statusBar := [status]]
        
        btnGetValue <- button pnlR [text := "Value"]
        btnGetType <- button pnlR [text := "Type"]
        btnGetKind <- button pnlR [text := "Kind"]
        
        search <- findReplaceDataCreate wxFR_DOWN
        
        let grrValue = GUIRRow btnGetValue txtValue
        let grrType = GUIRRow btnGetType txtType
        let grrKind = GUIRRow btnGetKind txtKind
        let guiRes = GUIRes grrValue grrType grrKind
        let guiCtx = GUICtx win lstPages lstModules txtCode guiRes status varTimer search 
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
        menuAppend mnuPage wxID_NEW "&New\tCtrl-n" "New Page" False
        menuAppend mnuPage wxID_CLOSE "&Close\tCtrl-w" "Close Page" False
        menuAppend mnuPage wxID_CLOSE_ALL "&Close All\tCtrl-Shift-w" "Close All Pages" False
        menuAppendSeparator mnuPage
        menuAppend mnuPage wxID_OPEN "&Open...\tCtrl-o" "Open Page" False
        menuAppend mnuPage wxID_SAVE "&Save\tCtrl-s" "Save Page" False
        menuAppend mnuPage wxID_SAVEAS "&Save as...\tCtrl-Shift-s" "Save Page as" False
        menuAppendSeparator mnuPage
        menuQuit mnuPage []
        
        mnuEdit <- menuPane [text := "Edit"]
        menuAppend mnuEdit wxID_UNDO "&Undo\tCtrl-z" "Undo" False
        menuAppend mnuEdit wxID_REDO "&Redo\tCtrl-Shift-z" "Redo" False
        menuAppendSeparator mnuEdit
        menuAppend mnuEdit wxID_CUT "C&ut\tCtrl-x" "Cut" False
        menuAppend mnuEdit wxID_COPY "&Copy\tCtrl-c" "Copy" False
        menuAppend mnuEdit wxID_PASTE "&Paste\tCtrl-v" "Paste" False
        menuAppendSeparator mnuEdit
        menuAppend mnuEdit wxID_FIND "&Find...\tCtrl-f" "Find" False
        menuAppend mnuEdit wxID_FORWARD "Find &Next\tCtrl-g" "Find Next" False
        menuAppend mnuEdit wxID_BACKWARD "Find &Previous\tCtrl-Shift-g" "Find Previous" False
        menuAppend mnuEdit wxID_REPLACE "&Replace...\tCtrl-Shift-r" "Replace" False

        mnuHask <- menuPane [text := "Haskell"]
        menuAppend mnuHask wxID_HASK_LOAD "&Load modules...\tCtrl-l" "Load Modules" False
        menuAppend mnuHask wxID_HASK_LOADNAME "Load modules by &name...\tCtrl-Shift-l" "Load Modules by Name" False
        menuAppend mnuHask wxID_HASK_RELOAD "&Reload\tCtrl-r" "Reload Modules" False
        menuAppendSeparator mnuHask
        menuAppend mnuHask wxID_HASK_EXTENSIONS "&Extensions...\tCtrl-Shift-x" "Configure Extensions" False
        menuAppendSeparator mnuHask
        menuAppend mnuHask wxID_HASK_VALUE "&Value\tCtrl-e" "Get the Value of the Current Expression" False
        menuAppend mnuHask wxID_HASK_TYPE "&Type\tCtrl-t" "Get the Type of the Current Expression" False
        menuAppend mnuHask wxID_HASK_KIND "&Kind\tCtrl-k" "Get the Kind of the Current Expression" False
        
        mnuHelp <- menuHelp []
        menuAbout mnuHelp [on command := infoDialog win "About hPage" "Author: Fernando Brujo Benavides"]
        
        set win [menuBar := [mnuPage, mnuEdit, mnuHask, mnuHelp]]
        evtHandlerOnMenuCommand win wxID_NEW $ onCmd "runHP' addPage" $ runHP' HP.addPage
        evtHandlerOnMenuCommand win wxID_CLOSE $ onCmd "runHP' closePage" $ runHP' HP.closePage
        evtHandlerOnMenuCommand win wxID_CLOSE_ALL $ onCmd "runHP' closeAllPages" $ runHP' HP.closeAllPages
        evtHandlerOnMenuCommand win wxID_OPEN $ onCmd "openPage" openPage
        evtHandlerOnMenuCommand win wxID_SAVE $ onCmd "savePage" savePage
        evtHandlerOnMenuCommand win wxID_SAVEAS $ onCmd "savePageAs" savePageAs
        evtHandlerOnMenuCommand win wxID_UNDO $ onCmd "runHP' undo" $ runHP' HP.undo
        evtHandlerOnMenuCommand win wxID_REDO $ onCmd "runHP' redo" $ runHP' HP.redo
        evtHandlerOnMenuCommand win wxID_CUT $ onCmd "cut" cut
        evtHandlerOnMenuCommand win wxID_COPY $ onCmd "copy" copy
        evtHandlerOnMenuCommand win wxID_PASTE $ onCmd "paste" paste
        evtHandlerOnMenuCommand win wxID_FIND $ onCmd "justFind" justFind
        evtHandlerOnMenuCommand win wxID_FORWARD $ onCmd "findNext" justFindNext
        evtHandlerOnMenuCommand win wxID_BACKWARD $ onCmd "findPrev" justFindPrev
        evtHandlerOnMenuCommand win wxID_REPLACE $ onCmd "findReplace" findReplace
        evtHandlerOnMenuCommand win wxID_HASK_LOAD $ onCmd "loadModules" loadModules
        evtHandlerOnMenuCommand win wxID_HASK_LOADNAME $ onCmd "loadModulesByName" loadModulesByName
        evtHandlerOnMenuCommand win wxID_HASK_RELOAD $ onCmd "reloadModules" reloadModules
        evtHandlerOnMenuCommand win wxID_HASK_EXTENSIONS $ onCmd "extensions" configureExtensions
        evtHandlerOnMenuCommand win wxID_HASK_VALUE $ onCmd "getValue" getValue
        evtHandlerOnMenuCommand win wxID_HASK_TYPE $ onCmd "getType" getType
        evtHandlerOnMenuCommand win wxID_HASK_KIND $ onCmd "getKind" getKind
        
        -- Tool bar...
        tbMain <- toolBarEx win True True []
        mitNew <- menuFindItem mnuPage wxID_NEW
        mitOpen <- menuFindItem mnuPage wxID_OPEN
        mitSave <- menuFindItem mnuPage wxID_SAVE
        mitCut <- menuFindItem mnuEdit wxID_CUT
        mitCopy <- menuFindItem mnuEdit wxID_COPY
        mitPaste <- menuFindItem mnuEdit wxID_PASTE
        mitReload <- menuFindItem mnuHask wxID_HASK_RELOAD
        newPath <- imageFile "new.png"
        openPath <- imageFile "open.png"
        savePath <- imageFile "save.png"
        cutPath <- imageFile "cut.png"
        copyPath <- imageFile "copy.png"
        pastePath <- imageFile "paste.png"
        reloadPath <- imageFile "reload.png"
        toolMenu tbMain mitNew "New" newPath [tooltip := "New"]
        toolMenu tbMain mitOpen "Open" openPath [tooltip := "Open"]
        toolMenu tbMain mitSave "Save" savePath [tooltip := "Save"]
        toolBarAddSeparator tbMain
        toolMenu tbMain mitCut "Cut" cutPath [tooltip := "Cut"]
        toolMenu tbMain mitCopy "Copy" copyPath [tooltip := "Copy"]
        toolMenu tbMain mitPaste "Paste" pastePath [tooltip := "Paste"]
        toolBarAddSeparator tbMain
        toolMenu tbMain mitReload "Reload" reloadPath [tooltip := "Reload Modules"]
        toolBarSetToolBitmapSize tbMain $ sz 32 32

        -- Layout settings
        let txtCodeL    = fill $ widget txtCode
            lstPagesL   = fill $ boxed "Pages" $ fill $ widget lstPages
            lstModulesL = fill $ boxed "Modules" $ fill $ widget lstModules
            valueRowL   = [widget btnGetValue, hfill $ widget txtValue]
            typeRowL    = [widget btnGetType, hfill $ widget txtType]
            kindRowL    = [widget btnGetKind, hfill $ widget txtKind]
            resultsGridL= hfill $ boxed "Expression" $ grid 5 0 [valueRowL, typeRowL, kindRowL]
            leftL       = container pnlL $ column 5 [lstPagesL, lstModulesL]
            rightL      = container pnlR $ column 5 [txtCodeL, resultsGridL]
        set win [layout := container pnl $ fill $ vsplit splLR 7 400 leftL rightL,
                 clientSize := sz 800 600]

        -- ...and RUN!
        refreshPage model guiCtx
        focusOn txtCode

-- EVENT HANDLERS --------------------------------------------------------------
refreshPage, savePageAs, savePage, openPage,
    pageChange, copy, cut, paste,
    justFind, justFindNext, justFindPrev, findReplace,
    restartTimer, killTimer,
    getValue, getType, getKind,
    loadModules, loadModulesByName, reloadModules,
    configureExtensions :: HPS.ServerHandle -> GUIContext -> IO ()

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

configureExtensions model guiCtx@GUICtx{guiWin = win, guiStatus = status} =
    do
        hpsRes <- tryIn model HP.getLanguageExtensions
        case hpsRes of
            Left err ->
                warningDialog win "Error" err
            Right exs ->
                do
                    res <- multiOptionsDialog win "Choose the language extensions to activate" "Extensions" (sort HP.availableExtensions) exs
                    case res of
                        Nothing ->
                            return ()
                        Just newexs ->
                            do
                                set status [text := "setting..."]
                                runHP (HP.setLanguageExtensions newexs) model guiCtx

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
                    textCtrlSetSelection txtCode (length s + ip) ip
                    refreshExpr model guiCtx False 

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
                    textCtrlSetSelection txtCode (length r + ip) ip
                    refreshExpr model guiCtx False
        
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
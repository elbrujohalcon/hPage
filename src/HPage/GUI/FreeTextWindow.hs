{-# LANGUAGE GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             FunctionalDependencies,
             UndecidableInstances #-}
             
module HPage.GUI.FreeTextWindow ( gui ) where

import Data.List
import Control.Monad
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.WXCore.Dialogs
-- import HPage.Stub.Control (HPage)
import qualified HPage.Stub.Control as HP hiding (HPage)
import qualified HPage.Stub.Server as HPS

gui :: IO ()
gui =
    do
        -- Server context
        model <- HPS.start 
        
        win <- frame [text := "hPage"]
        topLevelWindowSetIconFromFile win "../res/images/hpage.png"
        
        --HACK: closing with an exception avoids wxWidgets ugly warnings on OSX
        set win [on closing := undefined]
        
        -- Containers
        splLR <- splitterWindow win []
        pnlL <- panel splLR []
        pnlR <- panel splLR []
        splTB <- splitterWindow pnlR []
        pnlT <- panel splTB []
        pnlB <- panel splTB []
        
        -- Text page...
    --  txtCode <- styledTextCtrl win []
        txtCode <- textCtrlRich pnlT []
        
        -- Document Selector
        lstPages <- singleListBox pnlL []

        -- Results list
        lstResults <- listCtrl pnlB [columns := [("Expression", AlignLeft, 100),
                                                 ("Value", AlignLeft, 200),
                                                 ("Type", AlignLeft, 300)]]
        
        -- Status bar...
        status <- statusField [text := "hello... this is hPage! type in your instructions :)"]
        set win [statusBar := [status]]
        
        let onCmd acc = do
                            mustRefresh <- acc model win lstPages txtCode lstResults status
                            if mustRefresh
                                then display model win lstPages txtCode lstResults status
                                else return ()
        
        -- Events
        set lstPages [on select := onCmd $ pageChange]
        
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
        menuItem mnuEdit [text := "&Undo\tCtrl-z",         on command := onCmd $ say "undo"]
        menuItem mnuEdit [text := "&Redo\tCtrl-Shift-z",   on command := onCmd $ say "redo"]
        menuLine mnuEdit
        mitCut  <- menuItem mnuEdit [text := "C&ut\tCtrl-x",        on command := onCmd $ say "cut"]
        mitCopy <- menuItem mnuEdit [text := "&Copy\tCtrl-c",       on command := onCmd $ say "copy"]
        mitPaste <- menuItem mnuEdit [text := "&Paste\tCtrl-v",     on command := onCmd $ say "paste"]
        menuLine mnuEdit
        menuItem mnuEdit [text := "&Find...\tCtrl-f",               on command := onCmd $ say "find"]
        menuItem mnuEdit [text := "&Find Next\tCtrl-g",             on command := onCmd $ say "findNext"]

        mnuHask <- menuPane [text := "Haskell"]
        menuItem mnuHask [text := "&Load module...\tCtrl-l",        on command := onCmd $ say "load"]
        mitReload <- menuItem mnuHask [text := "&Reload\tCtrl-r",   on command := onCmd $ say "reload"]
        menuLine mnuHask
        mitRun <- menuItem mnuHask [text := "&Evaluate\tCtrl-e",    on command := onCmd $ say "eval"]
        menuItem mnuHask [text := "&Type\tCtrl-t",        on command := onCmd $ say "typeOf"]
        menuItem mnuHask [text := "&Kind\tCtrl-k",        on command := onCmd $ say "kindOf"]
        
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
        toolMenu tbMain mitRun "Run" "../res/images/run.png" [tooltip := "Run"]
        toolMenu tbMain mitReload "Reload" "../res/images/reload.png" [tooltip := "Reload"]
        
        -- Layout settings
        let pnlLLayout = fill $ widget lstPages
            pnlBLayout = fill $ widget lstResults
            pnlTLayout = fill $ widget txtCode
            pnlRLayout = fill $ hsplit splTB 7 100 (container pnlT pnlTLayout) (container pnlB pnlBLayout)
        set win [layout := (margin 1) $ fill $ vsplit splLR 7 0 (container pnlL pnlLLayout) (container pnlR pnlRLayout),
                    clientSize := sz 640 480]


        -- ...and RUN!
        display model win lstPages txtCode lstResults status
        focusOn txtCode
    where
        say x _model _win _lstPages _txtCode lstResults _status = itemAppend lstResults [x, "a value", "a type"] >> return False
        runHP hpacc model _ _ _ _ _ = HPS.runIn model hpacc >> return True

display :: (Textual w1, Selection w, Items w [Char]) =>
            HPS.ServerHandle -> t -> w -> w1 -> t1 -> t2 -> IO ()
display model _win lstPages txtCode _lstResults _status =
    do
        (ps, i, t) <- HPS.runIn model $ do
                                            pc <- HP.getPageCount
                                            pages <- mapM HP.getPageNthDesc [0..pc-1]
                                            ind <- HP.getPageIndex
                                            txt <- HP.getPageText
                                            return (pages, ind, txt)
        itemsDelete lstPages
        (flip mapM) ps $ \pd ->
                            let prefix = if HP.pIsModified pd
                                            then "*"
                                            else ""
                                name   = case HP.pPath pd of
                                             Nothing -> "new page"
                                             Just fn -> fn
                                p = HP.pIndex pd
                             in itemAppend lstPages $ prefix ++ name ++ "-" ++ show p
        set lstPages [selection := i]
        set txtCode [text := t] 

savePageAs, savePage, openPage, pageChange :: (Textual txt, Selection lst, Items lst [Char]) =>
                                                    HPS.ServerHandle -> Window frame -> lst -> txt -> t1 -> t2 -> IO Bool
pageChange model _ lstPages _ _ _ =
    do
        i <- get lstPages selection
        HPS.runIn model $ HP.setPageIndex i
        return True

openPage model win _ _ _ _ =
    do
        fileName <- fileOpenDialog win True True "Open file..." [("Haskells",["*.hs"]),
                                                                 ("Any file",["*.*"])] "" ""
        case fileName of
            Nothing ->
                return False
            Just f ->
                do
                    HPS.runIn model $ HP.openPage f
                    return True

savePageAs model win _ _ _ _ =
    do
        fileName <- fileSaveDialog win True True "Save file..." [("Haskells",["*.hs"]),
                                                                 ("Any file",["*.*"])] "" ""
        case fileName of
            Nothing ->
                return False
            Just f ->
                do
                    HPS.runIn model $ HP.savePageAs f
                    return True

savePage model win lstPages txtCode lstResults status =
    do
        path <- HPS.runIn model $ HP.getPagePath
        case path of
            Nothing ->
                savePageAs model win lstPages txtCode lstResults status
            _ ->
                do
                    HPS.runIn model HP.savePage
                    return True
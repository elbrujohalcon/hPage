
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
        pages <- varCreate ([] :: [HPS.ServerHandle])
        
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
        
        let onCmd acc = acc pages win lstPages txtCode lstResults status
        
        -- Events
        set lstPages [on select := onCmd $ pageChange]
        
        -- Menu bar...
        -- menuBar win []
        mnuPage <- menuPane [text := "Page"]
        mitNew  <- menuItem mnuPage [text := "&New\tCtrl-n",     on command := onCmd $ addPage]
        menuItem mnuPage [text := "&Close\tCtrl-w",   on command := onCmd $ closePage]
        menuItem mnuPage [text := "&Close All\tCtrl-Shift-w",   on command := onCmd $ closeAllPages]
        menuLine mnuPage
        mitOpen <- menuItem mnuPage [text := "&Open...\tCtrl-o", on command := onCmd $ say "open"]
        mitSave <- menuItem mnuPage [text := "&Save\tCtrl-s",    on command := onCmd $ say "save"]
        menuItem mnuPage [text := "&Save as...\tCtrl-Shift-s",   on command := onCmd $ say "save as"]
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
        onCmd $ addPage        
        focusOn txtCode
    where
        say x _pages _win _lstPages _txtCode lstResults _status = itemAppend lstResults [x, "a value", "a type"]
    
        setPage i pages _win lstPages  txtCode _lstResults _status
            | i < 0 =
                do
                    set lstPages [selection := -1]
                    set txtCode [text := "<- Select a page from your left"]
            | otherwise =
                do
                    set lstPages [selection := i]
                    page <- varGet pages >>= return . (!! i)
                    newText <- HPS.runIn page HP.getPageText
                    set txtCode [text := newText]
        
        addPage pages win lstPages txtCode lstResults status =
            do
                newPage <- HPS.start
                varUpdate pages (newPage:)
                itemAppend lstPages "new page"
                setPage 0 pages win lstPages txtCode lstResults status
        
        pageChange pages win lstPages txtCode lstResults status =
            do
                i <- get lstPages selection
                page <- varGet pages >>= return . (!! i)
                txt <- HPS.runIn page HP.getPageText
                HPS.runIn page $ HP.setPageText $ (show i) ++ txt
                setPage i pages win lstPages txtCode lstResults status
        
        closePage pages win lstPages txtCode lstResults status =
            do
                --TODO: Confirm if we want to close an unsaved page
                i <- get lstPages selection
                case i of
                    (-1) ->
                        return ()
                    0 ->
                        do
                            allPages <- varGet pages
                            let page = allPages !! i
                                pageCount = (length allPages) - 1
                            HPS.stop page
                            varUpdate pages $ deleteAt i
                            itemDelete lstPages i
                            case pageCount of
                                0 -> addPage pages win lstPages txtCode lstResults status
                                _ -> setPage 0 pages win lstPages txtCode lstResults status
                    _ ->
                        do
                            page <- varGet pages >>= return . (!! i)
                            HPS.stop page
                            varUpdate pages $ deleteAt i
                            itemDelete lstPages i
                            setPage (i-1) pages win lstPages txtCode lstResults status

        closeAllPages pages win lstPages txtCode lstResults status =
            do
                pageCount <- varGet pages >>= return . length
                forM_ [0..pageCount] $ \_ -> closePage pages win lstPages txtCode lstResults status

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = take i xs ++ drop (i+1) xs
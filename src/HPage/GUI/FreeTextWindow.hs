
module HPage.GUI.FreeTextWindow ( gui ) where

import Graphics.UI.WX
import Graphics.UI.WXCore.Dialogs
import HPage.Stub.Control (HPage)
import qualified HPage.Stub.Control as HP hiding (HPage)
import qualified HPage.Stub.Server as HPS

gui :: IO ()
gui = do
        server <- HPS.start
        drawWindow server

drawWindow :: HPS.ServerHandle -> IO ()
drawWindow server =
    do
        HPS.runIn server HP.eval
        
        frMain <- frame [text := "hPage"]
        
        --HACK: closing with an exception avoids wxWidgets ugly warnings on OSX
        set frMain [on closing := undefined]
        
        -- Containers
        ntbk <- notebook frMain []
        pnlFT <- panel ntbk []
        splFT <- splitterWindow pnlFT []
        
        -- Text page...
    --  txtCode <- styledTextCtrl frMain []
        txtCode <- textCtrlRich splFT []
        
        -- Results list
        listResults <- listCtrl splFT [columns := [("Expression", AlignLeft, 100),
                                                   ("Value", AlignLeft, 200),
                                                   ("Type", AlignLeft, 300)]]
        
        let say x = itemAppend listResults [x, "a value", "a type"]
        
        -- Status bar...
        status <- statusField [text := "hello... this is hPage! type in your instructions :)"]
        set frMain [statusBar := [status]]
                
        -- Menu bar...
        mnuPage <- menuPane [text := "Page"]
        mitNew <- menuItem mnuPage [text := "&New\tCtrl-n",
                                    on command := say "new"]
        menuItem mnuPage [text := "&Close\tCtrl-w",
                          on command := say "close"]
        menuLine mnuPage
        mitOpen <- menuItem mnuPage [text := "&Open...\tCtrl-o",
                                     on command := say "open"]
        mitSave <- menuItem mnuPage [text := "&Save\tCtrl-s",
                                     on command := say "save"]
        menuItem mnuPage [text := "&Save as...\tCtrl-Shift-s",
                          on command := say "save as"]
        menuLine mnuPage
        menuQuit mnuPage []
        
        mnuEdit <- menuPane [text := "Edit"]
        menuItem mnuEdit [text := "&Undo\tCtrl-z",
                          on command := say "undo"]
        menuItem mnuEdit [text := "&Redo\tCtrl-Shift-z",
                          on command := say "redo"]
        menuLine mnuEdit
        mitCut <- menuItem mnuEdit [text := "C&ut\tCtrl-x",
                                    on command := say "cut"]
        mitCopy <- menuItem mnuEdit [text := "&Copy\tCtrl-c",
                                     on command := say "copy"]
        mitPaste <- menuItem mnuEdit [text := "&Paste\tCtrl-v",
                                      on command := say "paste"]
        menuLine mnuEdit
        menuItem mnuEdit [text := "&Find...\tCtrl-f",
                          on command := say "find"]
        menuItem mnuEdit [text := "&Find Next\tCtrl-g",
                          on command := say "findNext"]

        mnuHask <- menuPane [text := "Haskell"]
        menuItem mnuHask [text := "&Load module...\tCtrl-l",
                          on command := say "load"]
        mitReload <- menuItem mnuHask [text := "&Reload\tCtrl-r",
                                       on command := say "reload"]
        menuLine mnuHask
        mitRun <- menuItem mnuHask [text := "&Evaluate\tCtrl-e",
                                    on command := say "eval"]
        menuItem mnuHask [text := "&Type\tCtrl-t",
                          on command := say "typeOf"]
        menuItem mnuHask [text := "&Kind\tCtrl-k",
                          on command := say "kindOf"]
        
        mnuHelp <- menuHelp []
        menuAbout mnuHelp [on command := infoDialog frMain "About hPage" "Author: Fernando Brujo Benavides"]
        
        set frMain [menuBar := [mnuPage, mnuEdit, mnuHask, mnuHelp]]
    
    
        -- Tool bar...
        tbMain <- toolBarEx frMain True True []
        toolMenu tbMain mitNew "New"  "../res/images/new.png" [tooltip := "New"]
        toolMenu tbMain mitOpen "Open" "../res/images/open.png" [tooltip := "Open"]
        toolMenu tbMain mitSave "Save" "../res/images/save.png" [tooltip := "Save"]
        toolMenu tbMain mitCut "Cut"  "../res/images/cut.png" [tooltip := "Cut"]
        toolMenu tbMain mitCopy "Copy" "../res/images/copy.png" [tooltip := "Copy"]
        toolMenu tbMain mitPaste "Paste" "../res/images/paste.png" [tooltip := "Paste"]
        toolMenu tbMain mitRun "Run" "../res/images/run.png" [tooltip := "Run"]
        toolMenu tbMain mitReload "Reload" "../res/images/reload.png" [tooltip := "Reload"]
            
        
        -- Layout settings
        set frMain [layout := (margin 1) $ fill $ 
                                tabs ntbk [tab "New Page" $ 
                                                container pnlFT $ (margin 1) $  
                                                    fill $ hsplit splFT 7 100 (fill $ widget txtCode) (fill $ widget listResults)],
                    clientSize := sz 640 480]
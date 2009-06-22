
module HPage.GUI.MainWindow (
    mainWindow
    ) where

import Control.Monad.Error
import Graphics.UI.WX
import Graphics.UI.WXCore.Dialogs
import HPage.Control (HPage)
import qualified HPage.Control as HP hiding (HPage)
import qualified HPage.Server as HPS

mainWindow :: IO ()
mainWindow = do
                server <- HPS.start
                drawWindow server

drawWindow :: HPS.ServerHandle -> IO ()
drawWindow server =
    do
    	frMain <- frame [text := "h page"]
    	
    	-- Text page...
    --	txtCode <- styledTextCtrl frMain []
    	txtCode <- textCtrlRich frMain []
    	
        
       -- Status bar...
        status <- statusField [text := "hello... this is hPage! type in your instructions :)"]
        set frMain [statusBar := [status]]
        
        let run :: Show b => HPage b -> IO ()
            run = runPage server frMain txtCode status
        
        -- Menu bar...
    	mnuPage <- menuPane [text := "Page"]
    	mitNew <- menuItem mnuPage [text := "&New\tCtrl-n",
                                    on command := run clearPage]
    	menuLine mnuPage
    	mitOpen <- menuItem mnuPage [text := "&Open...\tCtrl-o",
                                     on command := run $ openPage frMain]
    	mitSave <- menuItem mnuPage [text := "&Save\tCtrl-s",
                                     on command := run $ savePage frMain]
    	menuItem mnuPage [text := "&Save as...\tCtrl-Shift-s",
                          on command := run $ savePageAs frMain]
    	menuLine mnuPage
    	menuQuit mnuPage []
    	
    	mnuEdit <- menuPane [text := "Edit"]
    	menuItem mnuEdit [text := "&Undo\tCtrl-z",
                          on command := run undo]
    	menuItem mnuEdit [text := "&Redo\tCtrl-Shift-z",
                          on command := run redo]
    	menuLine mnuEdit
    	mitCut <- menuItem mnuEdit [text := "C&ut\tCtrl-x",
                                    on command := run cut]
    	mitCopy <- menuItem mnuEdit [text := "&Copy\tCtrl-c",
                                     on command := run copy]
    	mitPaste <- menuItem mnuEdit [text := "&Paste\tCtrl-v",
                                      on command := run paste]
    	menuLine mnuEdit
    	menuItem mnuEdit [text := "&Find...\tCtrl-f",
                          on command := run find]
    	menuItem mnuEdit [text := "&Find Next\tCtrl-g",
                          on command := run findNext]
    	menuItem mnuEdit [text := "&Replace...\tCtrl-h",
                          on command := run replace]

    	mnuHask <- menuPane [text := "Haskell"]
    	menuItem mnuHask [text := "&Load module...\tCtrl-l",
                          on command := run $ loadModule frMain]
    	mitReload <- menuItem mnuHask [text := "&Reload\tCtrl-r",
                                       on command := run reloadModules]
    	menuLine mnuHask
    	mitRun <- menuItem mnuHask [text := "&Evaluate\tCtrl-e",
                                    on command := run eval]
    	menuItem mnuHask [text := "&Kind\tCtrl-k",
                          on command := run kindOf]
    	menuItem mnuHask [text := "&Type\tCtrl-t",
                          on command := run typeOf]
    	
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
    	toolMenu tbMain mitRun "Evaluate" "../res/images/run.png" [tooltip := "Evaluate"]
    	toolMenu tbMain mitReload "Reload" "../res/images/reload.png" [tooltip := "Reload"]
            
    	
    	-- Layout settings
    	set frMain [layout := (margin 10) $ fill $ widget txtCode,
    				clientSize := sz 640 480]
                

--- THAT WAS THE SCREEN... NOW FOR THE REAL CODE -------------------------------
runPage :: (Textual text, Textual statusBar, Show b) =>
           HPS.ServerHandle -> Frame a -> text -> statusBar -> HPage b -> IO () 
runPage srv win textBox status action =
    do
        title <- get win text
        set status [text := "Processing..."]
        t <- get textBox text
        HPS.runIn srv $ HP.setText t 
        res <- HPS.runIn srv $ try action
        case res of
            Left err ->
                do
                    errorDialog win title (show err)
            Right s ->
                case show s of
                    "()" ->
                        do
                            newText <- HPS.runIn srv HP.getText
                            set textBox [text := newText]
                    _ ->                            
                        do
                            infoDialog win title $ show s
                            newText <- HPS.runIn srv HP.getText
                            set textBox [text := newText]
        set status [text := "Ready"]
    where try a = (a >>= return . Right) `catchError` (return . Left)


clearPage :: HPage ()
openPage, savePage, savePageAs :: Frame a -> HPage ()
undo, redo, cut, copy, paste, find, findNext, replace :: HPage ()
loadModule :: Frame a -> HPage ()
reloadModules :: HPage ()
eval, typeOf, kindOf :: HPage String

clearPage = HP.clearPage

openPage win =
    do
        fileName <- liftIO $ fileOpenDialog win True True "Open file..." [("Haskells",["*.hs"]),
                                                                          ("Any file",["*.*"])] "" ""
        case fileName of
            Nothing ->
                return ()
            Just f ->
                HP.openPage f

savePage win =
    do
        file <- HP.currentPage
        case file of
            Nothing ->
                savePageAs win
            Just f ->
                HP.savePage f

savePageAs win =
    do
        fileName <- liftIO $ fileSaveDialog win True True "Save file..." [("Haskells",["*.hs"]),
                                                                          ("Any file",["*.*"])] "" ""
        case fileName of
            Nothing ->
                return ()
            Just f ->
                HP.savePage f

undo = HP.undo
redo = HP.redo
cut = HP.cut
copy = HP.copy
paste = HP.paste
find = HP.find
findNext = HP.findNext
replace = HP.replace
eval = HP.eval
kindOf = HP.kindOf
typeOf = HP.typeOf

loadModule win =
    do
        fileName <- liftIO $ fileOpenDialog win True True "Open file..." [("Haskells",["*.hs"])] "" ""
        case fileName of
            Nothing ->
                return ()
            Just f ->
                HP.loadModule f

reloadModules = HP.reloadModules
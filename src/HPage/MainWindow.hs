
module HPage.MainWindow (
    mainWindow
    ) where

import Control.Monad
import Control.Monad.Trans
import Graphics.UI.WX
import Graphics.UI.WXCore.Dialogs
import Control.Concurrent.Process ( Process )
import qualified Control.Concurrent.Process as P
import qualified Language.Haskell.Interpreter.Server as HS
import qualified Language.Haskell.Interpreter.Server.Command as HS

data HpState = HPS { workingPath :: Maybe FilePath,
                     serverHandle :: P.Handle HS.Command }

type ClientProcess a = Process HS.Result a 

mainWindow :: IO ()
mainWindow = P.runHere clientProcess

clientProcess :: ClientProcess ()
clientProcess = do
                    server <- P.spawn HS.start
                    liftIO $ drawWindow $ HPS Nothing server

drawWindow :: HpState -> IO ()
drawWindow initState =
    do
        state <- varCreate initState
    
    	frMain <- frame [text := "h page"]
    	
    	-- Text page...
    --	txtCode <- styledTextCtrl frMain []
    	txtCode <- textCtrlRich frMain []
    	
        
       -- Status bar...
        status <- statusField [text := "hello... this is hPage! type in your instructions :)"]
        set frMain [statusBar := [status]]
        
        
        -- Menu bar...
    	mnuPage <- menuPane [text := "Page"]
    	mitNew  <- menuItem mnuPage [text := "&New\tCtrl-n",
                                     on command := newDocument state txtCode]
    	menuLine mnuPage
    	mitOpen <- menuItem mnuPage [text := "&Open...\tCtrl-o",
                                     on command := openDocument state frMain txtCode]
    	mitSave <- menuItem mnuPage [text := "&Save\tCtrl-s",
                                     on command := saveDocument state frMain txtCode]
    	menuItem mnuPage [text := "&Save as...\tCtrl-Shift-s",
                          on command := saveDocumentAs state frMain txtCode]
    	menuLine mnuPage
    	menuQuit mnuPage []
    	
    	mnuEdit <- menuPane [text := "Edit"]
    	menuItem mnuEdit [text := "&Undo\tCtrl-z",
                          on command := infoDialog frMain "Undo" "work in progress"]
    	menuItem mnuEdit [text := "&Redo\tCtrl-Shift-z",
                          on command := infoDialog frMain "Redo" "work in progress"]
    	menuLine mnuEdit
    	mitCut  <- menuItem mnuEdit [text := "C&ut\tCtrl-x",
                                     on command := infoDialog frMain "Cut" "work in progress"]
    	mitCopy <- menuItem mnuEdit [text := "&Copy\tCtrl-c",
                                     on command := infoDialog frMain "Copy" "work in progress"]
    	mitPaste <- menuItem mnuEdit [text := "&Paste\tCtrl-v",
                                      on command := infoDialog frMain "Paste" "work in progress"]
    	menuLine mnuEdit
    	menuItem mnuEdit [text := "&Find...\tCtrl-f",
                          on command := infoDialog frMain "Find" "work in progress"]
    	menuItem mnuEdit [text := "&Find Next\tCtrl-g",
                          on command := infoDialog frMain "Find Next" "work in progress"]
    	menuItem mnuEdit [text := "&Replace...\tCtrl-h",
                          on command := infoDialog frMain "Replace" "work in progress"]
    	menuLine mnuEdit
    	menuItem mnuEdit [text := "&Select All\tCtrl-a",
                          on command := infoDialog frMain "Select All" "work in progress"]
    
    	mnuHask <- menuPane [text := "Haskell"]
    	menuItem mnuHask [text := "&Load module...\tCtrl-l",
                          on command := loadModule state frMain status]
    	menuItem mnuHask [text := "&Browse modules\tCtrl-b",
                          on command := browseModules state frMain status]
    	mitReload <- menuItem mnuHask [text := "&Reload\tCtrl-r",
                                       on command := reloadModules state frMain status]
    	menuLine mnuHask
    	mitRun  <- menuItem mnuHask [text := "&Evaluate\tCtrl-e",
                                     on command := runSelection state status txtCode]
    	menuItem mnuHask [text := "&Kind\tCtrl-k",
                          on command := getKindOfSelection state status txtCode]
    	menuItem mnuHask [text := "&Type\tCtrl-t",
                          on command := getTypeOfSelection state status txtCode]
    	
    	mnuHelp <- menuHelp []
    	menuAbout mnuHelp [on command := infoDialog frMain "About hPage" "Author: Fernando Brujo Benavides"]
    	
    	set frMain [menuBar := [mnuPage, mnuEdit, mnuHask, mnuHelp]]
    
    
    	-- Tool bar...
    	tbMain <- toolBarEx frMain True True []
    	toolMenu tbMain mitNew "New"  "HPage/images/new.png" [tooltip := "New"]
    	toolMenu tbMain mitOpen "Open" "HPage/images/open.png" [tooltip := "Open"]
    	toolMenu tbMain mitSave "Save" "HPage/images/save.png" [tooltip := "Save"]
    	toolMenu tbMain mitCut "Cut"  "HPage/images/cut.png" [tooltip := "Cut"]
    	toolMenu tbMain mitCopy "Copy" "HPage/images/copy.png" [tooltip := "Copy"]
    	toolMenu tbMain mitPaste "Paste" "HPage/images/paste.png" [tooltip := "Paste"]
    	toolMenu tbMain mitRun "Evaluate" "HPage/images/run.png" [tooltip := "Evaluate"]
    	toolMenu tbMain mitReload "Reload" "HPage/images/reload.png" [tooltip := "Reload"]
            
    	
    	-- Layout settings
    	set frMain [layout := (margin 10) $ fill $ widget txtCode,
    				clientSize := sz 640 480]
                

--- THAT WAS THE SCREEN... NOW FOR THE REAL CODE -------------------------------

-- File functions --------------------------------------------------------------
cleanWorkingPath :: Var HpState -> IO HpState
cleanWorkingPath state = varUpdate state $ \s -> s { workingPath = Nothing }
modifyWorkingPath :: Var HpState -> FilePath -> IO HpState
modifyWorkingPath state f = varUpdate state $ \s -> s { workingPath = Just f } 

newDocument :: Textual t => Var HpState -> t -> IO ()
newDocument state textBox =
    do
        cleanWorkingPath state
        set textBox [text := ""]

openDocument :: Textual t => Var HpState -> Window a -> t -> IO ()
openDocument state frMain textBox =
    do
        fileName <- fileOpenDialog frMain True True "Open file..." [("Haskells",["*.hs"]),
                                                                    ("Any file",["*.*"])] "" ""
        case fileName of
            Nothing ->
                return ()
            Just f -> do
                        newText <- readFile f
                        set textBox [text := newText]
                        modifyWorkingPath state f
                        return ()

saveDocument :: Textual t => Var HpState -> Window a -> t -> IO ()
saveDocument state frMain textBox =
    do
        s <- varGet state
        case workingPath s of
            Nothing ->
                saveDocumentAs state frMain textBox
            Just f -> do
                        fileText <- get textBox text
                        writeFile f fileText

saveDocumentAs :: Textual t => Var HpState -> Window a -> t -> IO ()
saveDocumentAs state frMain textBox =
    do
        fileName <- fileSaveDialog frMain True True "Save file..." [("Haskells",["*.hs"]),
                                                                    ("Any file",["*.*"])] "" ""
        fileText <- get textBox text
        case fileName of
            Nothing ->
                return ()
            Just f -> do
                        writeFile f fileText
                        modifyWorkingPath state f
                        return ()

-- Haskell functions -----------------------------------------------------------
withServer :: Textual s => HS.CommandBuilder a -> a -> s -> Var HpState -> IO HS.Result
withServer cmdBuilder expr status state =
    do
        s <- varGet state
        set status [text := "Processing..."]
        res <- P.runHere $ do
                                me <- P.self
                                P.sendRecv (serverHandle s) $ cmdBuilder me expr
        set status [text := "Ready"]
        return res

withServerAndTextBox :: (Textual s, Textual t) => HS.CommandBuilder String -> t -> s -> Var HpState -> IO ()
withServerAndTextBox cmdBuilder textBox status state =
    do
        expr <- get textBox text
        ret <- withServer cmdBuilder expr status state
        case ret of
            Left e ->
                set textBox [text := (expr ++ " " ++ (show e))]
            Right s ->
                set textBox [text := (expr ++ " " ++ show s)]

withServerAndWindow :: Textual s => HS.CommandBuilder a -> a -> Frame b -> s -> Var HpState -> IO ()
withServerAndWindow cmdBuilder expr win status state =
    do
        ret <- withServer cmdBuilder expr status state
        case ret of
            Left e -> do
                        title <- liftIO $ get win text
                        errorDialog win title (show e)
            _ ->
                return ()


runSelection, getKindOfSelection, getTypeOfSelection :: (Textual s, Textual t) =>
                                                        Var HpState -> s -> t -> IO ()
runSelection state status textBox =
    withServerAndTextBox HS.eval textBox status state

getKindOfSelection state status textBox =
    withServerAndTextBox HS.kindOf textBox status state

getTypeOfSelection state status textBox =
    withServerAndTextBox HS.typeOf textBox status state

loadModule, browseModules, reloadModules :: Textual s => Var HpState -> Frame a -> s -> IO ()
loadModule state frMain status =
    do
        fileName <- fileOpenDialog frMain True True "Open file..." [("Haskells",["*.hs"])] "" ""
        case fileName of
            Nothing ->
                return ()
            Just f -> do
                        withServerAndWindow HS.loadModules [f] frMain status state
                        res <- withServer HS.getLoadedModules () status state
                        case res of
                            Left _ ->
                                return ()
                            Right ms ->
                                withServerAndWindow HS.setTopLevelModules (HS.unwrapModules ms) frMain status state

browseModules state frMain status =
    do
        modules <- withServer HS.getLoadedModules () status state
        case modules of
            Left e ->
                errorDialog frMain "Browse Modules" (show e)
            Right ms -> do
                result <- foldM (\acc m -> do
                                             exs <- withServer HS.getModuleExports m status state
                                             let thisRes = case exs of
                                                                Left err ->
                                                                    "Error: " ++ (show err)
                                                                Right elems ->
                                                                    "Module " ++ m ++ ":\n" ++ (showElems elems)
                                             return $ acc ++ thisRes ++ "\n")
                                "" (HS.unwrapModules ms)
                infoDialog frMain "Browse Modules" result
    where showElems mes = foldl (\res me ->
                                    res ++ "\t" ++ (show me) ++ "\n")
                                "" (HS.unwrapElems mes)

reloadModules state frMain status =
    do
        msRes <- withServer HS.getLoadedModules () status state
        case msRes of
            Left e ->
                errorDialog frMain "Reload Modules" (show e)
            Right ms -> do
                            withServerAndWindow HS.loadModules (HS.unwrapModules ms) frMain status state
                            withServerAndWindow HS.setTopLevelModules (HS.unwrapModules ms) frMain status state
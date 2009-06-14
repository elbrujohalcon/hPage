
module HPage.MainWindow (
    mainWindow
    ) where

import Control.Monad
import Graphics.UI.WX
import Graphics.UI.WXCore.Dialogs
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Server as HS

data HpState = HPS { workingPath :: Maybe FilePath,
                     serverHandle :: HS.ServerHandle }

mainWindow :: IO ()
mainWindow = do
                server <- HS.spawn
                drawWindow $ HPS Nothing server

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
withServer :: Textual statusBar => Hint.InterpreterT IO a -> statusBar -> Var HpState -> IO (Either Hint.InterpreterError a)
withServer action status state =
    do
        s <- varGet state
        set status [text := "Processing..."]
        res <- HS.runIn (serverHandle s) action
        set status [text := "Ready"]
        return res

withServerAndTextBox :: (Textual text, Textual statusBar, Show a) =>
                        (String -> Hint.InterpreterT IO a) -> text -> statusBar -> Var HpState -> IO ()
withServerAndTextBox actionBuilder textBox status state =
    do
        expr <- get textBox text
        ret <- withServer (actionBuilder expr) status state
        case ret of
            Left e ->
                set textBox [text := (expr ++ " " ++ (show e))]
            Right s ->
                set textBox [text := (expr ++ " " ++ show s)]

withServerAndWindow :: Textual statusBar => Hint.InterpreterT IO a -> Frame b -> statusBar -> Var HpState -> IO ()
withServerAndWindow action win status state =
    do
        ret <- withServer action status state
        case ret of
            Left e -> do
                        title <- get win text
                        errorDialog win title (show e)
            Right _ ->
                return ()


runSelection, getKindOfSelection, getTypeOfSelection :: (Textual s, Textual t) =>
                                                        Var HpState -> s -> t -> IO ()
runSelection state status textBox =
    withServerAndTextBox Hint.eval textBox status state

getKindOfSelection state status textBox =
    withServerAndTextBox Hint.kindOf textBox status state

getTypeOfSelection state status textBox =
    withServerAndTextBox Hint.typeOf textBox status state

loadModule, browseModules, reloadModules :: Textual s => Var HpState -> Frame a -> s -> IO ()
loadModule state frMain status =
    do
        fileName <- fileOpenDialog frMain True True "Open file..." [("Haskells",["*.hs"])] "" ""
        case fileName of
            Nothing ->
                return ()
            Just f -> do
                        withServerAndWindow (loadModules [f]) frMain status state
    where loadModules ms = do
                                Hint.loadModules ms
                                lms <- Hint.getLoadedModules
                                Hint.setTopLevelModules lms

browseModules state frMain status =
    do
        res <- withServer browseModules' status state
        case res of
            Left e ->
                errorDialog frMain "Browse Modules Error" (show e)
            Right ms ->
                infoDialog frMain "Browse Modules" ms
    where browseModules' = do
                              ms <- Hint.getLoadedModules
                              foldM (\acc m -> do
                                                    exs <- Hint.getModuleExports m
                                                    return $ acc ++ (showElems m exs) ++ "\n")
                                                 "" ms
          showElems m mes = "Module " ++ m ++ ":\n" ++
                            (foldl (\res me -> res ++ "\t" ++ (show me) ++ "\n") "" mes)

reloadModules state frMain status =
    withServerAndWindow reloadModules' frMain status state
    where reloadModules' = do
                                ms <- Hint.getLoadedModules
                                Hint.loadModules ms
                                Hint.setTopLevelModules ms
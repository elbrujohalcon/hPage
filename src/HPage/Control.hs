{-# LANGUAGE GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances,
             FunctionalDependencies,
             TypeSynonymInstances,
             UndecidableInstances #-} 
module HPage.Control (
    -- MONAD CONTROLS --
    HPage, evalHPage,
    -- PAGE CONTROLS --
    getPageIndex, setPageIndex, getPageCount,
    addPage, openPage, closePage, closeAllPages, getPagePath,
    savePage, savePageAs,
    isModifiedPage, isModifiedPageNth,
    closePageNth, getPageNthPath,
    savePageNth, savePageNthAs,
    PageDescription(..), getPageDesc, getPageNthDesc,
    -- SAFE PAGE CONTROLS --
    safeClosePage, safeCloseAllPages,
    safeSavePageAs, safeClosePageNth,
    safeSavePageNthAs,
    -- EXPRESSION CONTROLS --
    setPageText, getPageText, clearPage, 
    getExprIndex, setExprIndex, getExprCount,
    addExpr, removeExpr,
    setExprText, getExprText,
    removeNth,
    setExprNthText, getExprNthText,
    -- EDITION CONTROLS --
    undo, redo,
    -- HINT CONTROLS --
    interpret, interpretNth, Interpretation, intKind, intValue, intType, isIntType,
    valueOf, valueOfNth, kindOf, kindOfNth, typeOf, typeOfNth,
    loadModules,
    reloadModules, getLoadedModules,
    importModules, getImportedModules,
    getPackageModules,
    getModuleExports,
    getLanguageExtensions, setLanguageExtensions,
    getSourceDirs, setSourceDirs,
    getGhcOpts, setGhcOpts,
    loadPackage,
    loadModules', 
    reloadModules', getLoadedModules', importModules', getImportedModules',
    getModuleExports',
    getLanguageExtensions', setLanguageExtensions',
    getSourceDirs', setSourceDirs',
    getGhcOpts', setGhcOpts',
    reset, reset',
    cancel,
    Hint.InterpreterError, prettyPrintError,
    Hint.availableExtensions, Hint.Extension(..),
    ModuleDescription(..), ModuleElemDesc(..),
    -- DEBUG --
    ctxString
 ) where

import System.Directory
import System.FilePath
import Data.Set (Set, empty, union, fromList, toList)
import Control.Monad.Loops
import Control.Monad.Error
import Control.Monad.State
import Control.Concurrent.MVar
import Language.Haskell.Interpreter (OptionVal((:=)))
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Unsafe as Hint
import qualified Language.Haskell.Interpreter.Server as HS
import HPage.Utils.Log
import Data.List (isPrefixOf)
import qualified Data.ByteString.Char8 as Str
import qualified Language.Haskell.Exts.Parser as Parser
import Distribution.Simple.Configure hiding (tryGetConfigStateFile)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.Package
import Distribution.PackageDescription
import Distribution.ModuleName
import Distribution.Compiler

data Interpretation = Type {intKind ::  String} |
                      Expr {intValue :: String, intType :: String}
    deriving (Eq, Show)
    
isIntType :: Interpretation -> Bool
isIntType Type{} = True
isIntType Expr{} = False

data ModuleDescription = ModDesc {modName :: String,
                                  modInterpreted :: Bool}
    deriving (Eq)

instance Show ModuleDescription where
    show m = show (modName m, modInterpreted m)

data ModuleElemDesc = MEFun {funName :: String,
                             funType :: String} |
                      MEClass {clsName :: String,
                               clsFuns :: [ModuleElemDesc]} |
                      MEData {datName :: String,
                              datCtors :: [ModuleElemDesc]}
    deriving (Eq)

instance Show ModuleElemDesc where
    show MEFun{funName = fn, funType = []} = fn
    show MEFun{funName = fn, funType = ft} = fn ++ " :: " ++ ft
    show MEClass{clsName = cn, clsFuns = []} = "class " ++ cn
    show MEClass{clsName = cn, clsFuns = cfs} = "class " ++ cn ++ " where " ++ joinWith "\n" (map show cfs)
    show MEData{datName = dn, datCtors = []} = "data " ++ dn
    show MEData{datName = dn, datCtors = dcs} = "data " ++ dn ++ " = " ++ joinWith " | " (map show dcs)

data PageDescription = PageDesc {pIndex :: Int,
                                 pPath  :: Maybe FilePath,
                                 pIsModified :: Bool}
    deriving (Eq, Show)

newtype Expression = Exp {exprText :: String}       
    deriving (Eq, Show)

data InFlightData = LoadModules { loadingModules :: Set String,
                                  runningAction :: Hint.InterpreterT IO ()
                                  } |
                    ImportModules { importingModules :: Set String,
                                    runningAction :: Hint.InterpreterT IO ()
                                    } | 
                    SetSourceDirs { settingSrcDirs :: [FilePath],
                                    runningAction :: Hint.InterpreterT IO ()
                                    } |
                    SetGhcOpts { settingGhcOpts :: String,
                                 runningAction :: Hint.InterpreterT IO ()
                                 } |
                    Reset

data Page = Page { -- Display --
                   expressions :: [Expression],
                   currentExpr :: Int,
                   undoActions :: [HPage ()],
                   redoActions :: [HPage ()],
                   original :: [Expression],
                   -- File System --
                   filePath    :: Maybe FilePath
                  }

instance Show Page where
    show p = "Text: " ++ (showExpressions p) ++ 
           "\nFile: " ++ show (filePath p)
        where showExpressions pg = showWithCurrent (expressions pg) (currentExpr pg) "\n\n" $ ("["++) . (++"]")

data Context = Context { -- Package --
                         activePackage :: Maybe PackageIdentifier,
                         pkgModules :: [Hint.ModuleName],
                         -- Pages --
                         pages :: [Page],
                         currentPage :: Int,
                         -- Hint --
                         loadedModules :: Set String,
                         importedModules :: Set String,
                         extraSrcDirs :: [FilePath],
                         ghcOptions :: String,
                         server :: HS.ServerHandle,
                         running :: Maybe InFlightData,
                         recoveryLog :: Hint.InterpreterT IO () -- To allow cancelation of actions
                       }
 
instance Show Context where
    show c = showWithCurrent (pages c) (currentPage c) sep $ (top++) . (++bottom)
        where sep = "\n" ++ replicate 80 '-' ++ "\n"
              top = replicate 80 'v' ++ "\n"
              bottom = "\n" ++ replicate 80 '^'

newtype HPageT m a = HPT { state :: StateT Context m a }
    deriving (Monad, MonadIO, MonadTrans)

instance Monad m => MonadState Context (HPageT m) where
    get = HPT $ get
    put = HPT . put

instance MonadError e m => MonadError e (HPageT m) where
    throwError = lift . throwError
    catchError (HPT a) h = HPT $ a `catchError` (\e -> state $ h e)

type Extension = Hint.Extension

instance Ord Extension where
    e1 `compare` e2 = (show e1) `compare` (show e2)

type HPage = HPageT IO

evalHPage :: HPage a -> IO a
evalHPage hpt = do
                    hs <- liftIO $ HS.start
                    let nop = return ()
                    let emptyContext = Context Nothing [] [emptyPage] 0 empty (fromList ["Prelude"]) [] "" hs Nothing nop
                    (state hpt) `evalStateT` emptyContext


ctxString :: HPage String
ctxString = get >>= return . show

addPage :: HPage ()
addPage = modify (\ctx -> ctx{pages = emptyPage:(pages ctx),
                              currentPage = 0})

openPage :: FilePath -> HPage ()
openPage file = do
                    liftTraceIO $ "opening: " ++ file
                    s <- liftIO $ Str.readFile file
                    let str = Str.unpack s
                    let (newExprs, curExpr) = fromString' str $ length str
                        newPage = emptyPage{expressions = newExprs, 
                                            currentExpr = curExpr,
                                            filePath    = Just file,
                                            original    = newExprs}
                    modify (\ctx -> ctx{pages = newPage : pages ctx,
                                        currentPage = 0})

savePage :: HPage ()
savePage = get >>= savePageNth . currentPage

savePageNth :: Int -> HPage ()
savePageNth i = do
                    page <- getPageNth i
                    case filePath page of
                        Nothing ->
                            fail "No place to save"
                        Just file ->
                            savePageNthAs i file    

savePageAs :: FilePath -> HPage ()
savePageAs file = get >>=  (flip savePageNthAs) file . currentPage
 
savePageNthAs :: Int -> FilePath -> HPage ()
savePageNthAs i file = do
                            p <- getPageNth i
                            liftTraceIO $ "writing: " ++ file
                            liftIO $ Str.writeFile file $ Str.pack $ toString p  
                            modifyPageNth i (\page -> page{filePath = Just file,
                                                           original = (expressions page)})

isModifiedPage :: HPage Bool
isModifiedPage = get >>= isModifiedPageNth . currentPage

isModifiedPageNth :: Int -> HPage Bool
isModifiedPageNth i = withPageIndex i $ do
                                            page <- getPageNth i
                                            return $ expressions page /= original page 

getPagePath :: HPage (Maybe FilePath)
getPagePath = get >>= getPageNthPath . currentPage

getPageNthPath :: Int -> HPage (Maybe FilePath)
getPageNthPath i = getPageNth i >>= return . filePath

getPageCount :: HPage Int
getPageCount = get >>= return . length . pages

getPageIndex :: HPage Int
getPageIndex = get >>= return . currentPage

setPageIndex :: Int -> HPage ()
setPageIndex (-1) = modify (\ctx -> ctx{currentPage = (-1)})
setPageIndex i = withPageIndex i $ modify (\ctx -> ctx{currentPage = i})

getPageDesc :: HPage PageDescription
getPageDesc = get >>= getPageNthDesc . currentPage

getPageNthDesc :: Int -> HPage PageDescription
getPageNthDesc i = do
                        p <- getPageNthPath i
                        m <- isModifiedPageNth i
                        return $ PageDesc i p m

closePage :: HPage ()
closePage = get >>= closePageNth . currentPage

closePageNth :: Int -> HPage ()
closePageNth i = withPageIndex i $ do
                                        count <- getPageCount
                                        case count of
                                            1 ->
                                                closeAllPages
                                            _ ->
                                                modify (\c -> c{pages = insertAt i [] $ pages c,
                                                                currentPage = if i == currentPage c
                                                                                then case i of
                                                                                        0 -> 0
                                                                                        _ -> i - 1
                                                                                else currentPage c})

closeAllPages :: HPage ()
closeAllPages = modify (\ctx -> ctx{pages = [emptyPage],
                                    currentPage = 0})

safeClosePage :: HPage ()
safeClosePage = get >>= safeClosePageNth . currentPage 

safeCloseAllPages :: HPage ()
safeCloseAllPages = do
                        ps <- liftM (length . pages) $ get
                        ms <- anyM isModifiedPageNth [0..ps-1]
                        if ms then fail "There are modified pages"
                            else closeAllPages

safeClosePageNth :: Int -> HPage ()
safeClosePageNth i = do
                        m <- isModifiedPageNth i
                        if m then fail "The page is modified"
                            else closePageNth i 
                
safeSavePageAs :: FilePath -> HPage ()
safeSavePageAs file = get >>=  (flip safeSavePageNthAs) file . currentPage

safeSavePageNthAs :: Int -> FilePath -> HPage ()
safeSavePageNthAs i file = do
                                m <- liftIO $ doesFileExist file
                                if m then fail "The page is modified"
                                    else savePageNthAs i file

clearPage :: HPage ()
clearPage = setPageText "" 0 >> return ()

setPageText :: String -> Int -> HPage Bool
setPageText s ip = 
    do
        let (exprs, ix) = fromString' s ip
        page <- getPage
        if exprs /= expressions page || ix /= currentExpr page
            then
                do
                    modifyWithUndo (\p -> p{expressions = exprs,
                                            currentExpr = ix})
                    return True
            else
                return False

getPageText :: HPage String
getPageText = getPage >>= return . toString

getExprIndex :: HPage Int
getExprIndex = getPage >>= return . currentExpr

setExprIndex :: Int -> HPage ()
setExprIndex (-1) = modifyWithUndo (\p -> p{currentExpr = -1})
setExprIndex nth = withExprIndex nth $ modifyWithUndo (\p -> p{currentExpr = nth})

getExprCount :: HPage Int
getExprCount = getPage >>= return . length . expressions 

getExprText :: HPage String
getExprText = getPage >>= getExprNthText . currentExpr

setExprText :: String -> HPage ()
setExprText expr = getPage >>= flip setExprNthText expr . currentExpr

getExprNthText :: Int -> HPage String
getExprNthText nth = withExprIndex nth $ getPage >>= return . exprText . (!! nth) . expressions

setExprNthText :: Int -> String -> HPage ()
setExprNthText nth expr = withExprIndex nth $
                                do
                                    page <- getPage
                                    liftTraceIO ("setExprNthText",nth,expr,expressions page, currentExpr page)
                                    modifyWithUndo (\p ->
                                                        let newExprs = insertAt nth (fromString expr) $ expressions p
                                                            curExpr  = currentExpr p
                                                         in p{expressions = newExprs,
                                                              currentExpr = if curExpr < length newExprs then
                                                                                curExpr else
                                                                                length newExprs -1})
                        
addExpr :: String -> HPage ()
addExpr expr = do
                    p <- getPage
                    liftTraceIO ("addExpr",expr,expressions p, currentExpr p)
                    modifyWithUndo (\page ->
                                        let exprs = expressions page
                                            newExprs = insertAt (length exprs) (fromString expr) exprs
                                        in  page{expressions = newExprs,
                                                 currentExpr = length newExprs - 1})

removeExpr :: HPage ()
removeExpr = getPage >>= removeNth . currentExpr

removeNth :: Int -> HPage ()
removeNth i = setExprNthText i ""

undo, redo :: HPage ()
undo = do
            p <- getPage
            case undoActions p of
                [] ->
                    liftTraceIO ("not undo", expressions p)
                    -- return ()
                (acc:accs) ->
                    do
                        acc
                        getPage >>= (\px -> liftTraceIO ("redo added", expressions p, expressions px))
                        modifyPage (\page ->
                                        let redoAct = modifyPage (\pp -> pp{expressions = expressions p,
                                                                            currentExpr = currentExpr p})
                                         in page{redoActions = redoAct : redoActions page,
                                                 undoActions = accs})
redo = do
            p <- getPage
            case redoActions p of
                [] ->
                    liftTraceIO ("not redo", expressions p)
                    -- return ()
                (acc:accs) ->
                    do
                        acc
                        getPage >>= (\px -> liftTraceIO ("undo added", expressions px, expressions p))
                        modifyPage (\page ->
                                        let undoAct = modifyPage (\pp -> pp{expressions = expressions p,
                                                                            currentExpr = currentExpr p})
                                         in page{undoActions = undoAct : undoActions page,
                                                 redoActions = accs})

interpret :: HPage (Either Hint.InterpreterError Interpretation)
interpret = getPage >>= interpretNth . currentExpr
 
valueOf, kindOf, typeOf :: HPage (Either Hint.InterpreterError String)
valueOf = getPage >>= valueOfNth . currentExpr
kindOf = getPage >>= kindOfNth . currentExpr
typeOf = getPage >>= typeOfNth . currentExpr

interpretNth :: Int -> HPage (Either Hint.InterpreterError Interpretation)
interpretNth i =
        do
            typeRes <- typeOfNth i
            case typeRes of
                Left terr ->
                    do
                        kindRes <- kindOfNth i
                        case kindRes of
                            Left _ -> return $ Left terr
                            Right k -> return $ Right $ Type{intKind = k}
                Right t ->
                    do
                        valueRes <- valueOfNth i
                        case valueRes of
                            Left verr ->
                                let verrStr = show verr
                                in if isNotShowable verr
                                        then return $ Right $ Expr{intValue = "", intType = t}
                                        else return $ Left verr
                            Right v -> return $ Right $ Expr{intValue = v, intType = t}
        where isNotShowable (Hint.WontCompile ghcerrs) = any complainsAboutShow ghcerrs
              isNotShowable _ = False
              complainsAboutShow err = let errMsg = Hint.errMsg err
                                        in "No instance for (GHC.Show" `isPrefixOf` errMsg
              
valueOfNth, kindOfNth, typeOfNth :: Int -> HPage (Either Hint.InterpreterError String)
valueOfNth = runInExprNthWithLets Hint.eval
kindOfNth = runInExprNth Hint.kindOf
typeOfNth = runInExprNthWithLets Hint.typeOf

loadModules :: [String] -> HPage (Either Hint.InterpreterError ())
loadModules ms = do
                    let action = do
                                    liftTraceIO $ "loading: " ++ show ms
                                    Hint.loadModules ms
                                    Hint.getLoadedModules >>= Hint.setTopLevelModules
                    res <- syncRun action
                    case res of
                        Right _ ->
                            modify (\ctx -> ctx{loadedModules = union (fromList ms) (loadedModules ctx),
                                                recoveryLog = recoveryLog ctx >> action >> return ()})
                        Left e ->
                            liftErrorIO $ ("Error loading modules", ms, e)
                    return res

importModules :: [String] -> HPage (Either Hint.InterpreterError ())
importModules newms = do
                            ctx <- confirmRunning
                            let ms = toList $ importedModules ctx
                                action = do
                                            liftTraceIO $ "importing: " ++ show newms
                                            Hint.setImports $ ms ++ newms
                            res <- syncRun action
                            case res of
                                Right _ ->
                                    modify (\ctx -> ctx{importedModules = union (fromList newms) (importedModules ctx),
                                                        recoveryLog = recoveryLog ctx >> action >> return ()})
                                Left e ->
                                    liftErrorIO $ ("Error importing modules", ms, e)
                            return res

reloadModules :: HPage (Either Hint.InterpreterError ())
reloadModules = do
                    ctx <- confirmRunning
                    let ms = toList $ loadedModules ctx
                    syncRun $ do
                                liftTraceIO $ "reloading: " ++ (show ms)
                                Hint.loadModules ms
                                Hint.getLoadedModules >>= Hint.setTopLevelModules

getLoadedModules :: HPage (Either Hint.InterpreterError [ModuleDescription])
getLoadedModules = do
                        confirmRunning
                        syncRun $ do
                                    mns <- Hint.getLoadedModules
                                    mis <- mapM Hint.isModuleInterpreted mns
                                    return $ zipWith ModDesc mns mis 

getImportedModules :: HPage [Hint.ModuleName]
getImportedModules = confirmRunning >>= return . toList . importedModules 

getPackageModules :: HPage [Hint.ModuleName]
getPackageModules = confirmRunning >>= return . pkgModules

getModuleExports :: Hint.ModuleName -> HPage (Either Hint.InterpreterError [ModuleElemDesc])
getModuleExports mn = do
                            confirmRunning
                            let action = do
                                            exs <- Hint.getModuleExports mn
                                            mapM moduleElemDesc exs
                            syncRun action

getLanguageExtensions :: HPage (Either Hint.InterpreterError [Hint.Extension])
getLanguageExtensions = confirmRunning >> syncRun (Hint.get Hint.languageExtensions)

setLanguageExtensions :: [Hint.Extension] -> HPage (Either Hint.InterpreterError ())
setLanguageExtensions exs = confirmRunning >> syncRun (Hint.set [Hint.languageExtensions := exs])

getSourceDirs :: HPage [FilePath]
getSourceDirs = confirmRunning >>= return . extraSrcDirs

setSourceDirs :: [FilePath] -> HPage (Either Hint.InterpreterError ())
setSourceDirs ds =  do
                        let action = do
                                        liftTraceIO $ "setting src dirs: " ++ show ds
                                        Hint.unsafeSetGhcOption "-i"
                                        Hint.unsafeSetGhcOption "-i."
                                        forM_ ds $ Hint.unsafeSetGhcOption . ("-i" ++)
                        res <- syncRun action
                        case res of
                            Right _ ->
                                modify (\ctx -> ctx{extraSrcDirs = ds,
                                                    recoveryLog = recoveryLog ctx >> action >> return ()})
                            Left e ->
                                liftErrorIO $ ("Error setting source dirs", ds, e)
                        return res

getGhcOpts :: HPage String
getGhcOpts = confirmRunning >> get >>= return . ghcOptions

setGhcOpts :: String -> HPage (Either Hint.InterpreterError ())
setGhcOpts opts =  do
                        let action = do
                                        liftTraceIO $ "setting ghc opts: " ++ opts
                                        Hint.unsafeSetGhcOption opts
                        res <- syncRun action
                        case res of
                            Right _ ->
                                modify (\ctx -> ctx{ghcOptions = (ghcOptions ctx) ++ " " ++ opts,
                                                    recoveryLog = recoveryLog ctx >> action >> return ()})
                            Left e ->
                                liftErrorIO $ ("Error setting ghc opts dirs", opts, e)
                        return res

loadPackage :: FilePath -> HPage (Either String PackageIdentifier)
loadPackage file = do
                        let dir = dropFileName file
                        res <- liftIO $ tryGetPersistBuildConfig dir
                        case res of
                            Left err ->
                                return $ Left $ "Couldn't load package: " ++ err
                            Right lbinfo ->
                                do
                                    let pkgdesc = localPkgDescr lbinfo
                                        pkgname = package pkgdesc
                                        bldinfos= allBuildInfo pkgdesc
                                        dirs = ("dist" </> "build" </> "autogen") : (uniq $ concatMap hsSourceDirs bldinfos)
                                        exts = uniq . map (read . show) $ concatMap extensions bldinfos
                                        opts = uniq $ concatMap (hcOptions GHC) bldinfos
                                        mods = uniq . map (joinWith "." . components) $ exeModules pkgdesc ++ (libModules pkgdesc)
                                        action = do
                                                    liftTraceIO $ "loading package: " ++ show pkgname
                                                    Hint.unsafeSetGhcOption "-i"
                                                    Hint.unsafeSetGhcOption "-i."
                                                    forM_ dirs $ Hint.unsafeSetGhcOption . ("-i" ++)
                                                    Hint.set [Hint.languageExtensions := exts]
                                                    forM_ opts $ \opt -> Hint.unsafeSetGhcOption opt `catchError` (\_ -> return ())
                                                    return pkgname
                                    liftDebugIO mods
                                    res <- syncRun action
                                    case res of
                                        Right x ->
                                            do
                                                modify (\ctx -> ctx{activePackage       = Just pkgname,
                                                                    pkgModules          = mods,
                                                                    extraSrcDirs        = dirs,
                                                                    ghcOptions          = (ghcOptions ctx) ++ " " ++ (joinWith " " opts),
                                                                    recoveryLog         = recoveryLog ctx >> action >> return ()})
                                                return $ Right x
                                        Left e ->
                                            do
                                                liftErrorIO $ ("Error loading package", pkgname, e)
                                                return . Left $ prettyPrintError e

reset :: HPage (Either Hint.InterpreterError ())
reset = do
            res <- syncRun $ do
                                liftTraceIO $ "resetting"
                                Hint.reset
                                Hint.setImports ["Prelude"]
                                ms <- Hint.getLoadedModules
                                liftTraceIO $ "remaining modules: " ++ show ms
            case res of
                Right _ ->
                    modify (\ctx -> ctx{loadedModules = empty,
                                        importedModules = fromList ["Prelude"],
                                        extraSrcDirs = [],
                                        ghcOptions = "",
                                        running = Nothing,
                                        recoveryLog = return ()})
                Left e ->
                    liftErrorIO $ ("Error resetting", e)
            return res

loadModules' :: [String] -> HPage (MVar (Either Hint.InterpreterError ()))
loadModules' ms = do
                    let action = do
                                    liftTraceIO $ "loading': " ++ show ms
                                    Hint.loadModules ms
                                    Hint.getLoadedModules >>= Hint.setTopLevelModules
                    res <- asyncRun action
                    modify (\ctx -> ctx{running = Just $ LoadModules (fromList ms) action})
                    return res
                            
importModules' :: [String] -> HPage (MVar (Either Hint.InterpreterError ()))
importModules' newms = do
                            ctx <- confirmRunning
                            let ms = toList $ importedModules ctx
                                action = do
                                            liftTraceIO $ "importing': " ++ show newms
                                            Hint.setImports $ ms ++ newms
                            res <- asyncRun action
                            modify (\ctx -> ctx{running = Just $ ImportModules (fromList newms) action})
                            return res

reloadModules' :: HPage (MVar (Either Hint.InterpreterError ()))
reloadModules' = do
                    ctx <- confirmRunning
                    let ms = toList $ loadedModules ctx
                    asyncRun $ do
                                    liftTraceIO $ "reloading': " ++ (show ms)
                                    Hint.loadModules ms
                                    Hint.getLoadedModules >>= Hint.setTopLevelModules

getLoadedModules' :: HPage (MVar (Either Hint.InterpreterError [ModuleDescription]))
getLoadedModules' = do
                        confirmRunning
                        asyncRun $ do
                                        mns <- Hint.getLoadedModules
                                        mis <- mapM Hint.isModuleInterpreted mns
                                        return $ zipWith ModDesc mns mis
                                    
getImportedModules' :: HPage (MVar [Hint.ModuleName])
getImportedModules' = confirmRunning >>= liftIO . newMVar . toList . importedModules

getModuleExports' :: Hint.ModuleName -> HPage (MVar (Either Hint.InterpreterError [Hint.ModuleElem]))
getModuleExports' mn = confirmRunning >> asyncRun (Hint.getModuleExports mn)

getLanguageExtensions' :: HPage (MVar (Either Hint.InterpreterError [Hint.Extension]))
getLanguageExtensions' = confirmRunning >> asyncRun (Hint.get Hint.languageExtensions)

setLanguageExtensions' :: [Hint.Extension] -> HPage (MVar (Either Hint.InterpreterError ()))
setLanguageExtensions' exs = confirmRunning >> asyncRun (Hint.set [Hint.languageExtensions := exs])

getSourceDirs' :: HPage (MVar [FilePath])
getSourceDirs' = confirmRunning >>= liftIO . newMVar . extraSrcDirs

setSourceDirs' :: [FilePath] -> HPage (MVar (Either Hint.InterpreterError ()))
setSourceDirs' ds = do
                        let action = do
                                        liftTraceIO $ "setting src dirs: " ++ show ds
                                        Hint.unsafeSetGhcOption "-i"
                                        Hint.unsafeSetGhcOption "-i."
                                        forM_ ds $ Hint.unsafeSetGhcOption . ("-i" ++)
                        res <- asyncRun action
                        modify $ \ctx -> ctx{running = Just $ SetSourceDirs ds action}
                        return res

getGhcOpts' :: HPage (MVar String)
getGhcOpts' = confirmRunning >> get >>= liftIO . newMVar . ghcOptions

setGhcOpts' :: String -> HPage (MVar (Either Hint.InterpreterError ()))
setGhcOpts' opts =  do
                        let action = do
                                        liftTraceIO $ "setting ghc opts: " ++ opts
                                        Hint.unsafeSetGhcOption opts
                        res <- asyncRun action
                        modify $ \ctx -> ctx{running = Just $ SetGhcOpts opts action}
                        return res
                                
reset' :: HPage (MVar (Either Hint.InterpreterError ()))
reset' = do
            res <- asyncRun $ do
                                liftTraceIO $ "resetting'"
                                Hint.reset
                                Hint.setImports ["Prelude"]
                                ms <- Hint.getLoadedModules
                                liftTraceIO $ "remaining modules: " ++ show ms
            modify (\ctx -> ctx{running = Just Reset})
            return res

cancel :: HPage ()
cancel = do
            liftTraceIO $ "canceling"
            ctx <- get
            liftIO $ HS.stop $ server ctx
            hs <- liftIO $ HS.start
            liftIO $ HS.runIn hs $ recoveryLog ctx
            modify (\c -> c{server = hs,
                            running = Nothing})

prettyPrintError :: Hint.InterpreterError -> String
prettyPrintError (Hint.WontCompile ghcerrs)  = "Can't compile: " ++ (joinWith "\n" $ map Hint.errMsg ghcerrs)
prettyPrintError (Hint.UnknownError errStr) = "Unknown Error: " ++ errStr
prettyPrintError (Hint.NotAllowed errStr)   = "Not Allowed Action: " ++ errStr
prettyPrintError (Hint.GhcException errStr) = errStr

-- PRIVATE FUNCTIONS -----------------------------------------------------------
modifyPage :: (Page -> Page) -> HPage ()
modifyPage f = get >>= (flip modifyPageNth) f . currentPage

modifyPageNth :: Int -> (Page -> Page) -> HPage ()
modifyPageNth i f = withPageIndex i $ modify (\c ->
                                                let pgs = pages c
                                                    newPage = f $ pgs !! i
                                                 in c{pages = insertAt i [newPage] pgs})

getPage :: HPage Page
getPage = get >>= getPageNth . currentPage

getPageNth :: Int -> HPage Page
getPageNth i = withPageIndex i $ get >>= return . (!! i) . pages

withPageIndex :: Int -> HPage a -> HPage a
withPageIndex i acc = get >>= withIndex i acc . pages

withExprIndex :: Int -> HPage a -> HPage a
withExprIndex i acc = getPage >>= withIndex i acc . expressions

withIndex :: Show b => Int -> HPage a -> [b] -> HPage a
withIndex i acc is = case i of
                        -1 ->
                            fail "Nothing selected"
                        x | x >= length is ->
                            fail "Invalid index"
                        _ ->
                            acc 

runInExprNth :: (String -> Hint.InterpreterT IO String) -> Int -> HPage (Either Hint.InterpreterError String)
runInExprNth action i = do
                            page <- getPage
                            let exprs = expressions page
                            flip (withIndex i) exprs $ do
                                                            let expr = exprText $ exprs !! i
                                                            syncRun $ if "" == expr
                                                                        then return ""
                                                                        else action expr

runInExprNthWithLets :: (String -> Hint.InterpreterT IO String) -> Int -> HPage (Either Hint.InterpreterError String)
runInExprNthWithLets action i = do
                                    page <- getPage
                                    let exprs = expressions page
                                    flip (withIndex i) exprs $ let (b, item : a) = splitAt i exprs
                                                                   lets = filter isNamedExpr $ b ++ a
                                                                   expr = letsToString lets ++ exprText item
                                                                in syncRun $ if "" == exprText item
                                                                                then return ""
                                                                                else action expr

syncRun :: Hint.InterpreterT IO a -> HPage (Either Hint.InterpreterError a)
syncRun action = do
                    liftTraceIO "sync - running"
                    page <- confirmRunning
                    liftIO $ HS.runIn (server page) action

asyncRun :: Hint.InterpreterT IO a -> HPage (MVar (Either Hint.InterpreterError a)) 
asyncRun action = do
                    liftTraceIO "async - running"
                    page <- confirmRunning
                    liftIO $ HS.asyncRunIn (server page) action

confirmRunning :: HPage Context
confirmRunning = modify (\ctx -> apply (running ctx) ctx) >> get

apply :: Maybe InFlightData -> Context -> Context
apply Nothing      c = c
apply (Just Reset) c = c{loadedModules = empty,
                         importedModules = fromList ["Prelude"],
                         extraSrcDirs = [],
                         ghcOptions = "",
                         recoveryLog = return (),
                         running = Nothing}
apply (Just LoadModules{loadingModules = lms, runningAction = ra}) c =
    c{loadedModules = union lms (loadedModules c),
      recoveryLog   = (recoveryLog c) >> ra}
apply (Just ImportModules{importingModules = ims, runningAction = ra}) c =
    c{importedModules = union ims (importedModules c),
      recoveryLog     = (recoveryLog c) >> ra}
apply (Just SetSourceDirs{settingSrcDirs = ssds, runningAction = ra}) c =
    c{extraSrcDirs = ssds, recoveryLog = (recoveryLog c) >> ra}
apply (Just SetGhcOpts{settingGhcOpts = opts, runningAction = ra}) c =
    c{ghcOptions = (ghcOptions c) ++ " " ++ opts, recoveryLog = (recoveryLog c) >> ra}

fromString :: String -> [Expression]
fromString s = map Exp $ splitOn "\n\n" s

isNamedExpr :: Expression -> Bool
isNamedExpr e = case Parser.parseDecl (exprText e) of
                    Parser.ParseOk _ -> True
                    _ -> False 

fromString' :: String -> Int -> ([Expression], Int)
fromString' "" 0 = ([], -1)
fromString' s 0 = (fromString s, 0)
fromString' s i = (fromString s,
                   flip (-) 1 . length . splitOn "\n\n" $ take i s)

toString :: Page -> String
toString = joinWith "\n\n" . map exprText . expressions

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn [] xs = [xs]
splitOn _ [] = []
splitOn s xs = splitOn' [] s xs

splitOn' :: Eq a => [a] -> [a] -> [a] -> [[a]]
splitOn' [] _ [] = []
splitOn' acc _ [] = [reverse acc]
splitOn' acc s r@(x:xs)
    | isPrefixOf s r = (reverse acc) : splitOn' [] s (drop (length s) r)
    | otherwise = splitOn' (x:acc) s xs    

joinWith :: [a] -> [[a]] -> [a]
joinWith _ [] = []
joinWith sep (x:xs) = x ++ (concat . map (sep ++) $ xs)

insertAt :: Int -> [a] -> [a] -> [a]
insertAt 0 new [] = new
insertAt i new old | i == length old = old ++ new
                   | otherwise = let (before, (_:after)) = splitAt i old
                                  in before ++ new ++ after

showWithCurrent :: Show a => [a] -> Int -> String -> (String -> String) -> String
showWithCurrent allItems curItem sep mark = 
        drop (length sep) . concat $ map (showNth allItems curItem) [0..itemCount - 1]
    where itemCount  = length allItems
          showNth list sel cur = sep ++ ((if sel == cur then mark else id) $ show $ list !! cur)

modifyWithUndo :: (Page -> Page) -> HPage ()
modifyWithUndo f = modifyPage (\page ->
                                let newPage = f page
                                    undoAct = modifyPage (\pp -> pp{expressions = expressions page,
                                                                    currentExpr = currentExpr page})
                                 in newPage{undoActions = undoAct : undoActions page,
                                            redoActions = []}) 

emptyPage :: Page
emptyPage = Page [] (-1) [] [] [] Nothing

letsToString :: [Expression] -> String
letsToString [] = ""
letsToString exs = "let " ++ joinWith "; " (map exprText exs) ++ " in "

moduleElemDesc :: Hint.ModuleElem -> Hint.InterpreterT IO ModuleElemDesc
moduleElemDesc (Hint.Fun fn) = do
                                    t <- (Hint.typeOf fn) `catchError` (\e -> return [])
                                    return MEFun{funName = fn, funType = t}
moduleElemDesc (Hint.Class cn cfs) = do
                                        mcfs <- flip mapM cfs $ moduleElemDesc . Hint.Fun
                                        return MEClass{clsName = cn, clsFuns = mcfs}
moduleElemDesc (Hint.Data dn dcs) = do
                                        mdcs <- flip mapM dcs $ moduleElemDesc . Hint.Fun
                                        return MEData{datName = dn, datCtors = mdcs}
                                        
{- | Given a list, returns a new list with all duplicate elements removed.
For example:

>uniq "Mississippi" -> "Misp"

You should not rely on this function necessarily preserving order, though
the current implementation happens to.

This function is not compatible with infinite lists.

TAKEN FROM: http://hackage.haskell.org/packages/archive/MissingH/1.0.0/doc/html/src/Data-List-Utils.html#uniq -}
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/= x) xs)

{- Taken from the source code of Distribution.Simple.Configure -}
tryGetPersistBuildConfig :: FilePath -> IO (Either String LocalBuildInfo)
tryGetPersistBuildConfig distPref
    = tryGetConfigStateFile (localBuildInfoFile distPref)

tryGetConfigStateFile :: (Read a) => FilePath -> IO (Either String a)
tryGetConfigStateFile filename = do
  exists <- doesFileExist filename
  if not exists
    then return (Left missing)
    else withFileContents filename $ \str ->
      case lines str of
        [headder, rest] -> case reads rest of
            [(bi,_)] -> return (Right bi)
            _        -> return (Left cantParse)
        _            -> return (Left cantParse)
  where
    missing   = "Run the 'configure' command first."
    cantParse = "Saved package config file seems to be corrupt. "
             ++ "Try re-running the 'configure' command."
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
    getPageIndex, getPageText, setPageText,
    addPage, openPage, closePage, closeAllPages, getPagePath, setPageIndex,
    savePage, savePageAs, getPageCount,
    PageDescription(..), getPageNthDesc,
    
    -- EDITION CONTROLS --
    undo, redo,
    
    -- HINT CONTROLS --
    Hint.InterpreterError, prettyPrintError,
    getLanguageExtensions, setLanguageExtensions,
    getSourceDirs, setSourceDirs,
    getGhcOpts, setGhcOpts,
    getPackageModules,
    loadPackage, loadModules, getLoadedModules, reloadModules, 
    importModules, getImportedModules,
    getModuleExports, ModuleDescription(..), ModuleElemDesc(..),
    interpret, Interpretation, intKind, intValue, intValues, intResult,
    intType, isIntType, isIntExprs, isIntExpr, isIntIOExpr,
    Hint.availableExtensions,
    Hint.Extension(..),
    cancel,

    -- DEBUG --
    ctxString
 ) where

import System.Directory
import System.FilePath
import Data.Set (Set, empty, union, fromList, toList)
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
import qualified Language.Haskell.Exts as Xs
import Distribution.Simple.Configure hiding (tryGetConfigStateFile)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.Package
import Distribution.PackageDescription
import Distribution.ModuleName
import Distribution.Compiler
import qualified HPage.IOServer as HPIO
import Control.Exception(SomeException)

data Interpretation = Type  {intKind   :: String} |
                      Expr  {intValue  :: String,   intType :: String} |
                      IOExpr{intResult :: MVar (Either SomeException String), intType :: String} |
                      Exprs {intValues :: [String], intType :: String} 
    deriving (Eq)
instance Show Interpretation where
    show (Type x) = "type :: " ++ show x
    show (IOExpr _ t) = " io :: " ++ show t
    show (Expr x t) = x ++ show t
    show (Exprs xs t) = show xs ++ " :: [" ++ show t ++ "]"
    
isIntType :: Interpretation -> Bool
isIntType Type{}  = True
isIntType IOExpr{}= False
isIntType Expr{}  = False
isIntType Exprs{} = False

isIntExprs :: Interpretation -> Bool
isIntExprs Type{}  = False
isIntExprs IOExpr{}= False
isIntExprs Expr{}  = False
isIntExprs Exprs{} = True

isIntExpr :: Interpretation -> Bool
isIntExpr Type{}  = False
isIntExpr IOExpr{}= False
isIntExpr Expr{}  = True
isIntExpr Exprs{} = False

isIntIOExpr :: Interpretation -> Bool
isIntIOExpr Type{}  = False
isIntIOExpr IOExpr{}= True
isIntIOExpr Expr{}  = False
isIntIOExpr Exprs{} = False

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
                         recoveryLog :: Hint.InterpreterT IO (), -- To allow cancelation of actions
                         -- IO Server --
                         ioServer :: HPIO.ServerHandle
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
                    hpios <- liftIO $ HPIO.start
                    let nop = return ()
                    let emptyContext = Context Nothing [] [emptyPage] 0 empty (fromList ["Prelude"]) [] "" hs nop hpios
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
                    let (newExprs, curExpr) = exprFromString' str $ length str
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

setPageText :: String -> Int -> HPage Bool
setPageText s ip = 
    do
        let (exprs, ix) = exprFromString' s ip
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
 
interpretNth :: Int -> HPage (Either Hint.InterpreterError Interpretation)
interpretNth i =
        do
            liftDebugIO ("Interpreting", i)
            typeRes <- typeOfNth i
            case typeRes of
                Left terr ->
                    do
                        kindRes <- kindOfNth i
                        case kindRes of
                            Left _ -> return $ Left terr
                            Right k -> return . Right $ Type{intKind = k}
                Right t ->
                    do
                        if isIO t
                            then do
                                valueRes <- getIOFromExprNth i
                                case valueRes of
                                    Right ioAction ->
                                        do
                                            ctx <- get
                                            iores <- liftIO $ HPIO.runIn (ioServer ctx) $ ioAction
                                            return . Right $ IOExpr{intResult = iores, intType = t}
                                    Left err ->
                                        return $ Left err
                            else if isList t
                                    then do
                                            liftDebugIO "interpreting a list"
                                            valueRes <- getListFromExprNth i
                                            case valueRes of
                                                Left verr ->
                                                    if isNotShowable verr
                                                        then return $ Right $ Expr{intValue = "", intType = t}
                                                        else return $ Left verr
                                                Right list -> return . Right $ Exprs{intValues = list, intType = t}
                                    else do
                                            liftDebugIO "interpreting a value"
                                            valueRes <- valueOfNth i
                                            case valueRes of
                                                Left verr ->
                                                    if isNotShowable verr
                                                        then return $ Right $ Expr{intValue = "", intType = t}
                                                        else return $ Left verr
                                                Right v -> return $ Right $ Expr{intValue = v, intType = t}
        where isNotShowable (Hint.WontCompile ghcerrs) = any complainsAboutShow ghcerrs
              isNotShowable _ = False
              complainsAboutShow err = let errMsg = Hint.errMsg err
                                        in "No instance for (GHC.Show" `isPrefixOf` errMsg
              
valueOfNth, kindOfNth, typeOfNth :: Int -> HPage (Either Hint.InterpreterError String)
valueOfNth i = runInExprNthWithLets Hint.eval i
kindOfNth = runInExprNth Hint.kindOf
typeOfNth = runInExprNthWithLets Hint.typeOf

loadModules :: [String] -> HPage (Either Hint.InterpreterError ())
loadModules ms = do
                    prevctx <- get
                    let ims = toList $ importedModules prevctx
                        action = do
                                    liftTraceIO $ "loading: " ++ show ms
                                    Hint.loadModules ms
                                    Hint.setImports ims
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
                            ctx <- get
                            let ms = toList $ importedModules ctx
                                action = do
                                            liftTraceIO $ "importing: " ++ show newms
                                            Hint.setImports $ ms ++ newms
                            res <- syncRun action
                            case res of
                                Right _ ->
                                    modify (\c -> c{importedModules = union (fromList newms) (importedModules c),
                                                    recoveryLog = recoveryLog c >> action >> return ()})
                                Left e ->
                                    liftErrorIO $ ("Error importing modules", ms, e)
                            return res

reloadModules :: HPage (Either Hint.InterpreterError ())
reloadModules = do
                    ctx <- get
                    let ms = toList $ loadedModules ctx
                        ims = toList $ importedModules ctx
                    syncRun $ do
                                liftTraceIO $ "reloading: " ++ (show ms)
                                Hint.loadModules ms
                                Hint.setImports ims
                                Hint.getLoadedModules >>= Hint.setTopLevelModules

getLoadedModules :: HPage (Either Hint.InterpreterError [ModuleDescription])
getLoadedModules = syncRun $ do
                                mns <- Hint.getLoadedModules
                                mis <- mapM Hint.isModuleInterpreted mns
                                return $ zipWith ModDesc mns mis 

getImportedModules :: HPage [Hint.ModuleName]
getImportedModules = get >>= return . toList . importedModules 

getPackageModules :: HPage [Hint.ModuleName]
getPackageModules = get >>= return . pkgModules

getModuleExports :: Hint.ModuleName -> HPage (Either Hint.InterpreterError [ModuleElemDesc])
getModuleExports mn = syncRun $ do
                                    exs <- Hint.getModuleExports mn
                                    mapM moduleElemDesc exs

getLanguageExtensions :: HPage (Either Hint.InterpreterError [Hint.Extension])
getLanguageExtensions = get >> syncRun (Hint.get Hint.languageExtensions)

setLanguageExtensions :: [Hint.Extension] -> HPage (Either Hint.InterpreterError ())
setLanguageExtensions exs = get >> syncRun (Hint.set [Hint.languageExtensions := exs])

getSourceDirs :: HPage [FilePath]
getSourceDirs = get >>= return . extraSrcDirs

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
getGhcOpts = get >> get >>= return . ghcOptions

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
                                        libmods = case library pkgdesc of
                                                    Nothing -> []
                                                    Just l ->  libModules l
                                        exemods = concat (map exeModules $ executables pkgdesc)
                                        mods = uniq . map (joinWith "." . components) $ exemods ++ libmods
                                        action = do
                                                    liftTraceIO $ "loading package: " ++ show pkgname
                                                    Hint.unsafeSetGhcOption "-i"
                                                    Hint.unsafeSetGhcOption "-i."
                                                    forM_ dirs $ Hint.unsafeSetGhcOption . ("-i" ++)
                                                    Hint.set [Hint.languageExtensions := exts]
                                                    forM_ opts $ \opt -> Hint.unsafeSetGhcOption opt `catchError` (\_ -> return ())
                                                    return pkgname
                                    liftDebugIO mods
                                    res2 <- syncRun action
                                    case res2 of
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

cancel :: HPage ()
cancel = do
            liftTraceIO $ "canceling..."
            ctx <- get
            liftIO $ HS.stop $ server ctx
            liftIO $ HPIO.stop $ ioServer ctx
            hs <- liftIO $ HS.start
            hpios <- liftIO $ HPIO.start
            _ <- liftIO $ HS.runIn hs $ recoveryLog ctx
            modify (\c -> c{server      = hs,
                            ioServer    = hpios})

prettyPrintError :: Hint.InterpreterError -> String
prettyPrintError (Hint.WontCompile ghcerrs)  = "Can't compile: " ++ (joinWith "\n" $ map Hint.errMsg ghcerrs)
prettyPrintError (Hint.UnknownError errStr) = "Error: " ++ errStr
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

getIOFromExprNth :: Int -> HPage (Either Hint.InterpreterError (IO String))
getIOFromExprNth i =
    do
        page <- getPage
        let exprs = expressions page
        flip (withIndex i) exprs $ let (b, item : a) = splitAt i exprs
                                       lets = filter isNamedExpr $ b ++ a
                                       expr = "(" ++ letsToString lets ++ exprText item ++ ") >>= return . show"
                                    in syncRun $ Hint.interpret expr (Hint.as :: IO String)

getListFromExprNth :: Int -> HPage (Either Hint.InterpreterError [String])
getListFromExprNth i =
    do
        page <- getPage
        let exprs = expressions page
        flip (withIndex i) exprs $ let (b, item : a) = splitAt i exprs
                                       lets = filter isNamedExpr $ b ++ a
                                       expr = "map show (" ++ letsToString lets ++ exprText item ++ ")"
                                    in syncRun $ Hint.interpret expr (Hint.as :: [String])

runInExprNthWithLets :: (String -> Hint.InterpreterT IO String) -> Int -> HPage (Either Hint.InterpreterError String)
runInExprNthWithLets action i = do
                                    page <- getPage
                                    let exprs = expressions page
                                    flip (withIndex i) exprs $ let (b, item : a) = splitAt i exprs
                                                                   lets = filter isNamedExpr $ b ++ a
                                                                   expr = letsToString lets ++ exprText item
                                                                in do
                                                                        liftDebugIO ("runInExprNthWithLets", expr)
                                                                        syncRun $ if "" == exprText item
                                                                                    then return ""
                                                                                    else action expr

syncRun :: Hint.InterpreterT IO a -> HPage (Either Hint.InterpreterError a)
syncRun action = get >>= (\ctx -> liftIO $ HS.runIn (server ctx) action) 

exprFromString :: String -> [Expression]
exprFromString s = map Exp $ splitOn "\n\n" s

isNamedExpr :: Expression -> Bool
isNamedExpr e = case Xs.parseDecl (exprText e) of
                    Xs.ParseOk _ -> True
                    _ -> False

isType :: (Xs.Type -> Bool) -> String -> Bool
isType f t = case Xs.parseType t of
                    Xs.ParseOk ty -> f ty
                    _ -> False -- couldn't parse

isList :: String -> Bool
isList = isType isList'

isChar, isList' :: Xs.Type -> Bool
isList' (Xs.TyForall _ _ inttype)          = isList' inttype
isList' (Xs.TyParen inttype)               = isList' inttype
isList' (Xs.TyList inttype)                = not $ isChar inttype 
isList' (Xs.TyCon (Xs.Special Xs.ListCon)) = True
isList' _                                  = False

isChar (Xs.TyForall _ _ inttype)                = isChar inttype
isChar (Xs.TyParen inttype)                     = isChar inttype
isChar (Xs.TyCon (Xs.Qual _ (Xs.Ident "Char"))) = True 
isChar (Xs.TyCon (Xs.UnQual (Xs.Ident "Char"))) = True
isChar _                                        = False

isIO :: String -> Bool
isIO = isType isIO'

isIO'', isIO' :: Xs.Type -> Bool
isIO' (Xs.TyForall _ _ inttype) = isIO' inttype
isIO' (Xs.TyParen inttype)      = isIO' inttype
isIO' (Xs.TyApp inttype _)      = isIO'' inttype
isIO' _                         = False

isIO'' (Xs.TyForall _ _ inttype)              = isIO'' inttype
isIO'' (Xs.TyParen inttype)                   = isIO'' inttype
isIO'' (Xs.TyCon (Xs.Qual _ (Xs.Ident "IO"))) = True 
isIO'' (Xs.TyCon (Xs.UnQual (Xs.Ident "IO"))) = True
isIO'' _                                      = False

exprFromString' :: String -> Int -> ([Expression], Int)
exprFromString' "" 0 = ([], -1)
exprFromString' s 0 = (exprFromString s, 0)
exprFromString' s i = (exprFromString s,
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
letsToString exs = "let\n " ++ joinWith ";\n " (map (joinWith "\n  " . lines . exprText) exs) ++ "\n in "

moduleElemDesc :: Hint.ModuleElem -> Hint.InterpreterT IO ModuleElemDesc
moduleElemDesc (Hint.Fun fn) = do
                                    t <- (Hint.typeOf fn) `catchError` (\_ -> return [])
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
        [_header, rest] -> case reads rest of
            [(bi,_)] -> return (Right bi)
            _        -> return (Left cantParse)
        _            -> return (Left cantParse)
  where
    missing   = "Run the 'configure' command first."
    cantParse = "Saved package config file seems to be corrupt. "
             ++ "Try re-running the 'configure' command."

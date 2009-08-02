{-# LANGUAGE GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances,
             FunctionalDependencies,
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
    undo, redo, find, findNext,
    -- HINT CONTROLS --
    eval, evalNth, kindOf, kindOfNth, typeOf, typeOfNth,
    loadModule, reloadModules,
    eval', evalNth', kindOf', kindOfNth', typeOf', typeOfNth',
    loadModule', reloadModules',
    reset, reset',
    cancel,
    Hint.InterpreterError,
    -- DEBUG --
    ctxString
 ) where

import System.IO
import System.Directory
import Data.Set (Set, empty, insert, toList)
import Control.Monad.Loops
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.State.Class
import Control.Concurrent.MVar
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Server as HS
import Utils.Log
import List ((\\))
import qualified List as List
import qualified Data.ByteString.Char8 as Str
newtype Expression = Exp {asString :: String}
    deriving (Eq)

instance Show Expression where
    show = asString

data InFlightData = LoadModule { loadingModule :: FilePath,
                                 runningAction :: Hint.InterpreterT IO ()
                               } | Reset

data Page = Page { -- Display --
                   expressions :: [Expression],
                   currentExpr :: Int,
                   undoActions :: [HPage ()],
                   redoActions :: [HPage ()],
                   lastSearch  :: Maybe String,
                   original :: [Expression],
                   -- File System --
                   filePath    :: Maybe FilePath
                  }

instance Show Page where
    show p = "Text: " ++ (showExpressions p) ++ 
           "\nFile: " ++ show (filePath p)
        where showExpressions pg = showWithCurrent (expressions pg) (currentExpr pg) "\n\n" $ ("["++) . (++"]")

data Context = Context { -- Pages --
                         pages :: [Page],
                         currentPage :: Int,
                         -- Hint --
                         loadedModules :: Set FilePath,
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


type HPage = HPageT IO

evalHPage :: HPage a -> IO a
evalHPage hpt = do
                    hs <- liftIO $ HS.start
                    let nop = return ()
                    let emptyContext = Context [emptyPage] 0 empty hs Nothing nop
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
                    let newExprs = fromString $ Str.unpack s
                        newPage = emptyPage{expressions = newExprs, 
                                            currentExpr = if (length newExprs) == 0 then (-1) else 0,
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
setPageIndex i = withPageIndex i $ modify (\ctx -> ctx{currentPage = i})

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
clearPage = setPageText ""

setPageText :: String -> HPage ()
setPageText s = let exprs = fromString s
                 in modifyWithUndo (\page -> page{expressions = exprs,
                                                  currentExpr = (length exprs - 1)})

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
setExprText expr = do
                     page <- getPage
                     setExprNthText (currentExpr page) expr

getExprNthText :: Int -> HPage String
getExprNthText nth = withExprIndex nth $ getPage >>= return . show . (!! nth) . expressions

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
 
find :: String -> HPage ()
find text = do
                page <- getPage
                modifyPage (\p -> p{lastSearch = Just text})
                case nextMatching text page of
                    Nothing ->
                        return ()
                    Just i ->
                        setExprIndex i

findNext :: HPage ()
findNext = do
                page <- getPage
                case lastSearch page of
                    Nothing ->
                        return ()
                    Just text ->
                        find text

eval, kindOf, typeOf :: HPage (Either Hint.InterpreterError String)
eval = getPage >>= evalNth . currentExpr
kindOf = getPage >>= kindOfNth . currentExpr
typeOf = getPage >>= typeOfNth . currentExpr

evalNth, kindOfNth, typeOfNth :: Int -> HPage (Either Hint.InterpreterError String)
evalNth = runInExprNth Hint.eval
kindOfNth = runInExprNth Hint.kindOf
typeOfNth = runInExprNth Hint.typeOf

loadModule :: FilePath -> HPage (Either Hint.InterpreterError ())
loadModule f = do
                    let action = do
                                    liftTraceIO $ "loading: " ++ f
                                    Hint.loadModules [f]
                                    Hint.getLoadedModules >>= Hint.setTopLevelModules
                    res <- syncRun action
                    case res of
                        Right _ ->
                            modify (\ctx -> ctx{loadedModules = insert f (loadedModules ctx),
                                                recoveryLog = recoveryLog ctx >> action >> return ()})
                        Left e ->
                            liftErrorIO $ ("Error loading module", f, e)
                    return res

reloadModules :: HPage (Either Hint.InterpreterError ())
reloadModules = do
                    ctx <- confirmRunning
                    let ms = toList $ loadedModules ctx
                    syncRun $ do
                                liftTraceIO $ "reloading: " ++ (show ms)
                                Hint.loadModules ms
                                Hint.getLoadedModules >>= Hint.setTopLevelModules

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
                                        recoveryLog = return ()})
                Left e ->
                    liftErrorIO $ ("Error resetting", e)
            return res

eval', kindOf', typeOf' :: HPage (MVar (Either Hint.InterpreterError String))
eval' = getPage >>= evalNth' . currentExpr
kindOf' = getPage >>= kindOfNth' . currentExpr
typeOf' = getPage >>= typeOfNth' . currentExpr

evalNth', kindOfNth', typeOfNth' :: Int -> HPage (MVar (Either Hint.InterpreterError String))
evalNth' = runInExprNth' Hint.eval
kindOfNth' = runInExprNth' Hint.kindOf
typeOfNth' = runInExprNth' Hint.typeOf

loadModule' :: FilePath -> HPage (MVar (Either Hint.InterpreterError ()))
loadModule' f = do
                    let action = do
                                    liftTraceIO $ "loading': " ++ f
                                    Hint.loadModules [f]
                                    Hint.getLoadedModules >>= Hint.setTopLevelModules
                    res <- asyncRun action
                    modify (\ctx -> ctx{running = Just $ LoadModule f action})
                    return res
                            

reloadModules' :: HPage (MVar (Either Hint.InterpreterError ()))
reloadModules' = do
                    page <- confirmRunning
                    let ms = toList $ loadedModules page
                    asyncRun $ do
                                    liftTraceIO $ "reloading': " ++ (show ms)
                                    Hint.loadModules ms
                                    Hint.getLoadedModules >>= Hint.setTopLevelModules

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
            hs <- liftIO $ HS.start
            liftIO $ HS.runIn hs $ recoveryLog ctx
            --TODO: The current discarded server needs to be stopped here
            modify (\c -> c{server = hs,
                            running = Nothing})
            

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

withIndex :: Int -> HPage a -> [b] -> HPage a
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
                        case i of
                            -1 ->
                                fail "Nothing selected"
                            x | x >= length exprs ->
                                fail "Invalid index"
                            _ ->
                                do
                                    let expr = asString $ exprs !! i
                                    syncRun $ action expr

runInExprNth' :: (String -> Hint.InterpreterT IO String) -> Int -> HPage (MVar (Either Hint.InterpreterError String))
runInExprNth' action i = do
                        page <- getPage
                        let exprs = expressions page
                        case i of
                            -1 ->
                                fail "Nothing selected"
                            _ ->
                                do
                                    let expr = asString $ exprs !! i
                                    asyncRun $ action expr

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
                         recoveryLog = return (),
                         running = Nothing}
apply (Just lm)    c = c{loadedModules = insert (loadingModule lm) (loadedModules c),
                         recoveryLog   = (recoveryLog c) >> (runningAction lm)}

fromString :: String -> [Expression]
fromString = filter (/= Exp "") . map toExp . splitOn "" . lines
    where toExp = Exp . joinWith "\n"

toString :: Page -> String
toString = joinWith "\n\n" . map show . expressions

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep = reverse . (map reverse) . (foldl (\(acc:accs) el ->
                                                if el == sep
                                                then []:acc:accs
                                                else (el:acc):accs)
                                         [[]])

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

nextMatching :: String -> Page -> Maybe Int
nextMatching t p = let c = currentExpr p
                       es = expressions p
                       oes = rotate (c+1) $ zip [0..length es] es
                    in case List.find (include t) oes of
                            Nothing ->
                                Nothing;
                            Just (i, _) ->
                                Just i
    where rotate n xs = drop n xs ++ take n xs
          include x (_, Exp xs) = (xs \\ x) /= xs

emptyPage :: Page
emptyPage = Page [] (-1) [] [] Nothing [] Nothing
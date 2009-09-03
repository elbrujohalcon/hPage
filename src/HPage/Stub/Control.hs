{-# LANGUAGE GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances,
             FunctionalDependencies,
             UndecidableInstances #-} 

module HPage.Stub.Control (
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
    valueOf, valueOfNth, kindOf, kindOfNth, typeOf, typeOfNth,
    loadModules, reloadModules, getLoadedModules,
    valueOf', valueOfNth', kindOf', kindOfNth', typeOf', typeOfNth',
    loadModules', reloadModules', getLoadedModules',
    reset, reset',
    cancel,
    InterpreterError, prettyPrintError,
    -- DEBUG --
    ctxString
 ) where

import System.IO
import System.Directory
import Data.Set (Set, empty, union, fromList, toList)
import Data.Char
import Control.Monad.Loops
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.State.Class
import Control.Concurrent.MVar
import Utils.Log
import List (isPrefixOf)
import qualified List as List
import qualified Data.ByteString.Char8 as Str

data PageDescription = PageDesc {pIndex :: Int,
                                 pPath  :: Maybe FilePath,
                                 pIsModified :: Bool}
    deriving (Eq, Show)

newtype Expression = Exp {exprText :: String}
    deriving (Eq, Show)

type InterpreterError = String

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

data Context = Context { -- Pages --
                         pages :: [Page],
                         currentPage :: Int,
                         -- Hint --
                         loadedModules :: Set FilePath
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
evalHPage hpt = (state hpt) `evalStateT` (Context [emptyPage] 0 empty)


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
 
valueOf, kindOf, typeOf :: HPage (Either InterpreterError String)
valueOf = getPage >>= valueOfNth . currentExpr
kindOf = getPage >>= kindOfNth . currentExpr
typeOf = getPage >>= typeOfNth . currentExpr

valueOfNth, kindOfNth, typeOfNth :: Int -> HPage (Either InterpreterError String)
valueOfNth = runInExprNth "valueOf"
kindOfNth = runInExprNth "kindOf"
typeOfNth = runInExprNth "typeOf"

loadModules :: [String] -> HPage (Either InterpreterError ())
loadModules ms = modify (\ctx -> ctx{loadedModules = union (fromList ms) (loadedModules ctx)}) >>= return . Right

reloadModules :: HPage (Either InterpreterError ())
reloadModules = return $ Right ()

getLoadedModules :: HPage (Either InterpreterError [String])
getLoadedModules = get >>= return . Right . toList . loadedModules

reset :: HPage (Either InterpreterError ())
reset = modify (\ctx -> ctx{loadedModules = empty}) >>= return . Right

valueOf', kindOf', typeOf' :: HPage (MVar (Either InterpreterError String))
valueOf' = valueOf >>= liftIO . newMVar
kindOf' = kindOf >>= liftIO . newMVar
typeOf' = typeOf >>= liftIO . newMVar

valueOfNth', kindOfNth', typeOfNth' :: Int -> HPage (MVar (Either InterpreterError String))
valueOfNth' = runInExprNth' "valueOf"
kindOfNth' = runInExprNth' "kindOf"
typeOfNth' = runInExprNth' "typeOf"

loadModules' :: [String] -> HPage (MVar (Either InterpreterError ()))
loadModules' ms = loadModules ms >>= liftIO . newMVar

reloadModules' :: HPage (MVar (Either InterpreterError ()))
reloadModules' = reloadModules >>= liftIO . newMVar

getLoadedModules' :: HPage (MVar (Either InterpreterError [String]))
getLoadedModules' = getLoadedModules >>= liftIO . newMVar

reset' :: HPage (MVar (Either InterpreterError ()))
reset' = reset >>= liftIO . newMVar

cancel :: HPage ()
cancel = return ()
            

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
                        	do
                                liftErrorIO x
                                fail "Invalid index"
                        _ ->
                            acc 

runInExprNth :: String -> Int -> HPage (Either InterpreterError String)
runInExprNth action i = do
                            page <- getPage
                            let exprs = expressions page
                            case i of
                                -1 ->
                                    fail "Nothing selected"
                                x | x >= length exprs ->
                                    do
                                        liftErrorIO x
                                        fail "Invalid index"
                                _ ->
                                    do
                                        let expr = exprText $ exprs !! i
                                        return . Right $ action ++ ": " ++ expr

runInExprNth' :: String -> Int -> HPage (MVar (Either InterpreterError String))
runInExprNth' action i = do
                            page <- getPage
                            let exprs = expressions page
                            case i of
                                -1 ->
                                    fail "Nothing selected"
                                _ ->
                                    do
                                        let expr = exprText $ exprs !! i
                                        liftIO . newMVar . Right $ action ++ ": " ++ expr

fromString :: String -> [Expression]
fromString s = map Exp $ splitOn "\n\n" s

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

prettyPrintError :: InterpreterError -> String
prettyPrintError = show
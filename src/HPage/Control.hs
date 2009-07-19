{-# LANGUAGE GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances,
             FunctionalDependencies,
             UndecidableInstances #-} 

module HPage.Control (
    -- MONAD CONTROLS --
    HPage, evalHPage,
    -- PAGE CONTROLS -- 
    clearPage, openPage, savePage, currentPage,
    -- EDITING CONTROLS --
    setText, getText,
    getExprIndex, setExprIndex, getExpr, setExpr,
    addExpr, removeExpr, removeNth, getNth, setNth,
    undo, redo, find, findNext,
    -- HINT CONTROLS --
    eval, evalNth, kindOf, kindOfNth, typeOf, typeOfNth,
    loadModule, reloadModules,
    eval', evalNth', kindOf', kindOfNth', typeOf', typeOfNth',
    loadModule', reloadModules',
    reset, reset',
    cancel
 ) where

import System.IO
import Data.Set (Set, empty, insert, toList)
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.State.Class
import Control.Concurrent.MVar
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Server as HS
import Utils.Log

newtype Expression = Exp {asString :: String}
    deriving (Eq)

instance Show Expression where
    show = asString

data InFlightData = LoadModule { loadingModule :: FilePath,
                                 runningAction :: Hint.InterpreterT IO ()
                               } | Reset

data Page = Page { expressions :: [Expression],
                   currentExpr :: Int,
                   filePath :: Maybe FilePath,
                   loadedModules :: Set FilePath,
                   server :: HS.ServerHandle,
                   running :: Maybe InFlightData,
                   recoveryLog :: Hint.InterpreterT IO () -- To allow cancelation of actions
                 }

instance Show Page where
    show p = "Text: " ++ (showExpressions p) ++ 
           "\nFile: " ++ show (filePath p)

newtype HPageT m a = HPT { state :: StateT Page m a }
    deriving (Monad, MonadIO, MonadTrans)

instance Monad m => MonadState Page (HPageT m) where
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
                    let emptyPage = Page [] (-1) Nothing empty hs Nothing nop
                    (state hpt) `evalStateT` emptyPage

clearPage :: HPage ()
clearPage = modify (\page -> page{expressions = fromString "",
                                  currentExpr = -1,
                                  filePath = Nothing})

openPage :: FilePath -> HPage ()
openPage file = do
                    liftTraceIO $ "opening: " ++ file
                    s <- liftIO $ readFile file
                    modify (\page -> page{filePath = Just file})
                    setText s

savePage :: FilePath -> HPage ()
savePage file = do
                    p <- get
                    liftTraceIO $ "writing: " ++ file
                    liftIO $ writeFile file $ toString p  
                    modify (\page -> page{filePath = Just file})

currentPage :: HPage (Maybe FilePath)
currentPage = get >>= return . filePath 



setText :: String -> HPage ()
setText s = let exprs = fromString s in
                modify (\page -> page{expressions = exprs,
                                      currentExpr = (length exprs - 1)})

getText :: HPage String
getText = get >>= return . toString

getExprIndex :: HPage Int
getExprIndex = get >>= return . currentExpr

setExprIndex :: Int -> HPage ()
setExprIndex nth = do
                        page <- get
                        if (length (expressions page) >= nth && nth >= 0) then 
                            modify (\p -> p{currentExpr = nth}) else
                            fail "Invalid index" 

getExpr :: HPage String
getExpr = get >>= getNth . currentExpr

setExpr :: String -> HPage ()
setExpr expr = do
                    page <- get
                    setNth (currentExpr page) expr

getNth :: Int -> HPage String
getNth nth = do
                page <- get
                if (length (expressions page) >= nth && nth >= 0) then
                    return . show . (!! nth) . expressions $ page else
                    fail "Invalid index" 

setNth :: Int -> String -> HPage ()
setNth nth expr = do
                    page <- get
                    if (length (expressions page) >= nth && nth >= 0) then
                        modify (\p -> p{expressions = replaceExpression nth expr $ expressions p}) else
                        fail "Invalid index"
                        
addExpr :: String -> HPage ()
addExpr expr = modify (\page ->
                            let exprs = expressions page
                            in  page{expressions = replaceExpression (length exprs) expr exprs})

removeExpr :: HPage ()
removeExpr = get >>= removeNth . currentExpr

removeNth :: Int -> HPage ()
removeNth = flip setNth $ "" 

undo, redo :: HPage ()
undo = undefined
redo = undefined

find, findNext :: HPage ()
find = undefined
findNext = undefined




eval, kindOf, typeOf :: HPage (Either Hint.InterpreterError String)
eval = get >>= evalNth . currentExpr
kindOf = get >>= kindOfNth . currentExpr
typeOf = get >>= typeOfNth . currentExpr

evalNth, kindOfNth, typeOfNth :: Int -> HPage (Either Hint.InterpreterError String)
evalNth = runInNth Hint.eval
kindOfNth = runInNth Hint.kindOf
typeOfNth = runInNth Hint.typeOf

loadModule :: FilePath -> HPage (Either Hint.InterpreterError ())
loadModule f = do
                    let action = do
                                    liftTraceIO $ "loading: " ++ f
                                    Hint.loadModules [f]
                                    Hint.getLoadedModules >>= Hint.setTopLevelModules
                    res <- syncRun action
                    case res of
                        Right _ ->
                            modify (\p -> p{loadedModules = insert f (loadedModules p),
                                            recoveryLog = recoveryLog p >> action >> return ()})
                        Left e ->
                            liftErrorIO $ ("Error loading module", f, e)
                    return res

reloadModules :: HPage (Either Hint.InterpreterError ())
reloadModules = do
                    page <- confirmRunning
                    let ms = toList $ loadedModules page
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
                    modify (\p -> p{loadedModules = empty,
                                    recoveryLog = return ()})
                Left e ->
                    liftErrorIO $ ("Error resetting", e)
            return res

eval', kindOf', typeOf' :: HPage (MVar (Either Hint.InterpreterError String))
eval' = get >>= evalNth' . currentExpr
kindOf' = get >>= kindOfNth' . currentExpr
typeOf' = get >>= typeOfNth' . currentExpr

evalNth', kindOfNth', typeOfNth' :: Int -> HPage (MVar (Either Hint.InterpreterError String))
evalNth' = runInNth' Hint.eval
kindOfNth' = runInNth' Hint.kindOf
typeOfNth' = runInNth' Hint.typeOf

loadModule' :: FilePath -> HPage (MVar (Either Hint.InterpreterError ()))
loadModule' f = do
                    let action = do
                                    liftTraceIO $ "loading': " ++ f
                                    Hint.loadModules [f]
                                    Hint.getLoadedModules >>= Hint.setTopLevelModules
                    res <- asyncRun action
                    modify (\p -> p{running = Just $ LoadModule f action})
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
            modify (\p -> p{running = Just Reset})
            return res

cancel :: HPage ()
cancel = do
            liftTraceIO $ "canceling"
            page <- get
            hs <- liftIO $ HS.start
            liftIO $ HS.runIn hs $ recoveryLog page
            --TODO: The current discarded server needs to be stopped here
            modify (\p -> p{server = hs,
                            running = Nothing})
            

-- PRIVATE FUNCTIONS -----------------------------------------------------------
runInNth :: (String -> Hint.InterpreterT IO String) -> Int -> HPage (Either Hint.InterpreterError String)
runInNth action i = do
                        page <- get
                        let exprs = expressions page
                        case i of
                            -1 ->
                                fail "Nothing selected"
                            _ ->
                                do
                                    let expr = asString $ exprs !! i
                                    syncRun $ action expr

runInNth' :: (String -> Hint.InterpreterT IO String) -> Int -> HPage (MVar (Either Hint.InterpreterError String))
runInNth' action i = do
                        page <- get
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

confirmRunning :: HPage Page
confirmRunning = modify (\p -> apply (running p) p) >> get

apply :: Maybe InFlightData -> Page -> Page
apply Nothing      p = p
apply (Just Reset) p = p{loadedModules = empty,
                         recoveryLog = return (),
                         running = Nothing}
apply (Just lm)    p = p{loadedModules = insert (loadingModule lm) (loadedModules p),
                         recoveryLog   = (recoveryLog p) >> (runningAction lm)}

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

replaceExpression :: Int -> String -> [Expression] -> [Expression]
replaceExpression i expr exprs =
    let (before, (_:after)) = splitAt i exprs
        newExprs = fromString expr
    in before ++ newExprs ++ after
        
        
showExpressions :: Page -> String
showExpressions p = drop 2 . concat $ map (showNth allExps current) [0..expNum - 1]
    where allExps = expressions p
          current = currentExpr p
          expNum  = length allExps
          showNth list sel cur = "\n\n" ++ if sel == cur then
                                                "[" ++ show (list !! cur) ++ "]"
                                           else
                                                show $ list !! cur
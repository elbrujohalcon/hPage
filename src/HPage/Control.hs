{-# LANGUAGE GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances,
             FunctionalDependencies,
             UndecidableInstances #-} 

module HPage.Control (
    HPage, evalHPage,
    setText, getText,
    clearPage, openPage, savePage, currentPage,
    undo, redo, cut, copy, paste,
    find, findNext, replace,
    eval, evalNth, kindOf, kindOfNth, typeOf, typeOfNth,
    loadModule, reloadModules,
    eval', evalNth', kindOf', kindOfNth', typeOf', typeOfNth',
    loadModule', reloadModules',
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

newtype Expression = Exp {asString :: String}
    deriving (Eq)

instance Show Expression where
    show = asString

data Page = Page { expressions :: [Expression],
                   currentExpr :: Int,
                   filePath :: Maybe FilePath,
                   loadedModules :: Set FilePath,
                   server :: HS.ServerHandle,
                   backupServer :: HS.ServerHandle,
                   actionInFlight :: Hint.InterpreterT IO (),
                   history :: Hint.InterpreterT IO () }

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
                    hsb <- liftIO $ HS.start
                    let nop = return ()
                    let emptyPage = Page [] (-1) Nothing empty hs hsb nop nop
                    (state hpt) `evalStateT` emptyPage 

setText :: String -> HPage ()
setText s = let exprs = fromString s in
                modify (\page -> page{expressions = exprs,
                                      currentExpr = (length exprs - 1)})

getText :: HPage String
getText = get >>= return . toString

clearPage :: HPage ()
clearPage = modify (\page -> page{expressions = fromString "",
                                  filePath = Nothing})

openPage :: FilePath -> HPage ()
openPage file = do
                    s <- liftIO $ readFile file
                    modify (\page -> page{expressions = fromString s,
                                              filePath = Just file})

savePage :: FilePath -> HPage ()
savePage file = do
                    p <- get
                    liftIO $ writeFile file $ toString p  
                    modify (\page -> page{filePath = Just file})

currentPage :: HPage (Maybe FilePath)
currentPage = get >>= return . filePath 

undo, redo :: HPage ()
undo = undefined
redo = undefined

cut, copy, paste :: HPage ()
cut = undefined
copy = undefined
paste = undefined

find, findNext, replace :: HPage ()
find = undefined
findNext = undefined
replace = undefined

eval, kindOf, typeOf :: HPage String
eval = get >>= evalNth . currentExpr 
kindOf = get >>= kindOfNth . currentExpr 
typeOf = get >>= typeOfNth . currentExpr 

evalNth, kindOfNth, typeOfNth :: Int -> HPage String
evalNth = runInNth Hint.eval
kindOfNth = runInNth Hint.kindOf
typeOfNth = runInNth Hint.typeOf

loadModule :: FilePath -> HPage ()
loadModule f = do
                    res <- runIn $ do
                                        Hint.loadModules [f]
                                        ms <- Hint.getLoadedModules
                                        Hint.setTopLevelModules ms
                    modify (\page ->
                                let mods = loadedModules page in
                                    page{loadedModules = insert f mods})
                    return res
                            

reloadModules :: HPage ()
reloadModules = do
                    page <- get
                    let ms = toList $ loadedModules page
                    runIn $ do
                                Hint.loadModules ms
                                newMs <- Hint.getLoadedModules
                                Hint.setTopLevelModules newMs

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
           
                    res <- runIn' $ do
                                        Hint.loadModules [f]
                                        ms <- Hint.getLoadedModules
                                        Hint.setTopLevelModules ms
                    modify (\page ->
                                let mods = loadedModules page in
                                    page{loadedModules = insert f mods})
                    return res
                            

reloadModules' :: HPage (MVar (Either Hint.InterpreterError ()))
reloadModules' = do
                    page <- get
                    let ms = toList $ loadedModules page
                    runIn' $ do
                                Hint.loadModules ms
                                newMs <- Hint.getLoadedModules
                                Hint.setTopLevelModules newMs

cancel :: HPage ()
cancel = do
            page <- get
            let (bServ, hist) = (backupServer page, history page)
            hsb <- liftIO $ HS.start
            liftIO $ HS.flush bServ
            liftIO $ HS.asyncRunIn hsb hist
            --TODO: The current discarded server needs to be stopped here
            let newPage = page{server = bServ,
                               backupServer = hsb,
                               actionInFlight = return ()}
            put newPage
                                
            

-- PRIVATE FUNCTIONS -----------------------------------------------------------
runIn :: Hint.InterpreterT IO a -> HPage ()
runIn action = syncRun action >> return ()
    
runIn' :: Hint.InterpreterT IO a -> HPage (MVar (Either Hint.InterpreterError a))
runIn' = asyncRun

runInNth :: (String -> Hint.InterpreterT IO String) -> Int -> HPage String
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

syncRun :: Hint.InterpreterT IO a -> HPage a
syncRun action = do
                    page <- updateBackupServer action
                    let serv = server page
                    res <- liftIO $ HS.runIn serv action
                    case res of
                        Left e ->
                            fail $ show e
                        Right s ->
                            return s

asyncRun :: Hint.InterpreterT IO a -> HPage (MVar (Either Hint.InterpreterError a)) 
asyncRun action = do
                    page <- updateBackupServer action
                    let serv = server page
                    liftIO $ HS.asyncRunIn serv action

updateBackupServer :: Hint.InterpreterT IO a -> HPage Page
updateBackupServer action = do
                                page <- get
                                let (bServ, flyAcc, hist) = (backupServer page,
                                                             actionInFlight page,
                                                             history page)
                                liftIO $ HS.asyncRunIn bServ flyAcc
                                let newPage = page{actionInFlight = action >> return (),
                                                   history = hist >> flyAcc} 
                                put newPage
                                return newPage
                                     
                            
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

showExpressions :: Page -> String
showExpressions p = drop 2 . concat $ map (showNth allExps current) [0..expNum - 1]
    where allExps = expressions p
          current = currentExpr p
          expNum  = length allExps
          showNth list sel cur = "\n\n" ++ if sel == cur then
                                                "[" ++ show (list !! cur) ++ "]"
                                           else
                                                show $ list !! cur
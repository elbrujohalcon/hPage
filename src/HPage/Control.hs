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
import Data.Set (Set, empty, insert, toList, member)
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
                    let nop = liftIO $ putStrLn "Acata!"
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
                    liftIO $ putStrLn $ "opening: " ++ file
                    s <- liftIO $ readFile file
                    modify (\page -> page{expressions = fromString s,
                                              filePath = Just file})

savePage :: FilePath -> HPage ()
savePage file = do
                    p <- get
                    liftIO $ putStrLn $ "writing: " ++ file
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

eval, kindOf, typeOf :: HPage (Either Hint.InterpreterError String)
eval = get >>= evalNth . currentExpr 
kindOf = get >>= kindOfNth . currentExpr 
typeOf = get >>= typeOfNth . currentExpr 

evalNth, kindOfNth, typeOfNth :: Int -> HPage (Either Hint.InterpreterError String)
evalNth = runInNth Hint.eval
kindOfNth = runInNth Hint.kindOf
typeOfNth = runInNth Hint.typeOf

loadModule :: FilePath -> HPage ()
loadModule f = do
                    page <- get
                    let mods = loadedModules page
                    case member f mods of
                        False ->
                            do
                                res <- runIn $ do
                                                    liftIO $ putStrLn $ "loading: " ++ f
                                                    Hint.loadModules [f]
                                                    ms <- Hint.getLoadedModules
                                                    Hint.setTopLevelModules ms
                                put $ page{loadedModules = insert f mods}
                                return res
                        True ->
                            return ()
                            

reloadModules :: HPage ()
reloadModules = do
                    page <- get
                    let ms = toList $ loadedModules page
                    runIn $ do
                                liftIO $ putStrLn $ "reloading: " ++ (show ms)
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
                    page <- get
                    let mods = loadedModules page
                    case member f mods of
                        False ->
                            do
                                res <- runIn' $ do
                                                    liftIO $ putStrLn $ "loading': " ++ f
                                                    Hint.loadModules [f]
                                                    ms <- Hint.getLoadedModules
                                                    Hint.setTopLevelModules ms
                                put $ page{loadedModules = insert f mods}
                                return res
                        True ->
                            liftIO $ newMVar $ Right ()
                            

reloadModules' :: HPage (MVar (Either Hint.InterpreterError ()))
reloadModules' = do
                    page <- get
                    let ms = toList $ loadedModules page
                    runIn' $ do
                                liftIO $ putStrLn $ "reloading': " ++ (show ms)
                                Hint.loadModules ms
                                newMs <- Hint.getLoadedModules
                                Hint.setTopLevelModules newMs

cancel :: HPage ()
cancel = do
            liftIO $ putStrLn $ "cancelling"
            page <- get
            let (bServ, hist) = (backupServer page, history page)
            hsb <- liftIO $ HS.start
            liftIO $ HS.flush bServ
            liftIO $ HS.asyncRunIn hsb hist
            --TODO: The current discarded server needs to be stopped here
            modify (\p -> p{server = bServ,
                            backupServer = hsb,
                            actionInFlight = return ()})
            

-- PRIVATE FUNCTIONS -----------------------------------------------------------
runIn :: Hint.InterpreterT IO a -> HPage ()
runIn action = syncRun action >> return ()
    
runIn' :: Hint.InterpreterT IO a -> HPage (MVar (Either Hint.InterpreterError a))
runIn' = asyncRun

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
                    updres <- updateBackupServer
                    case updres of
                        Right _ ->
                            do                                     
                                modify (\p -> p{actionInFlight = (action >> (liftIO $ putStrLn "jjj")),
                                                history = (history p >> actionInFlight p)})
                                page <- get
                                let serv = server page
                                liftIO $ HS.runIn serv $ do
                                                            ms <- Hint.getLoadedModules
                                                            liftIO $ putStrLn $ "S:" ++ show ms
                                                            action
                        Left err ->
                            return $ Left err

asyncRun :: Hint.InterpreterT IO a -> HPage (MVar (Either Hint.InterpreterError a)) 
asyncRun action = do
                    updateBackupServer
                    modify (\p -> p{actionInFlight = (action >> (liftIO $ putStrLn "jjj")),
                                    history = (history p >> actionInFlight p)})
                    page <- get
                    let serv = server page
                    liftIO $ HS.asyncRunIn serv action

updateBackupServer :: HPage (Either Hint.InterpreterError ())
updateBackupServer = do
                        page <- get
                        let bServ = backupServer page
                        let flyAcc  = actionInFlight page
                        liftIO $ HS.runIn bServ $ do
                                                    ms <- Hint.getLoadedModules
                                                    liftIO $ putStrLn $ "BS:" ++ show ms
                                                    flyAcc
                        
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
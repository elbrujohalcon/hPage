
{-# LANGUAGE GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             TypeSynonymInstances,
             FlexibleInstances,
             FunctionalDependencies,
             UndecidableInstances #-} 

module HPage.Stub.Control (
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
    cancel,
    InterpreterError
 ) where

import System.IO
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.State.Class
import Control.Concurrent.MVar

newtype HPageT m a = HPT { state :: StateT String m a }
    deriving (Monad, MonadIO, MonadTrans)
    
instance Monad m => MonadState String (HPageT m) where
    get = HPT $ get
    put = HPT . put

instance MonadError e m => MonadError e (HPageT m) where
    throwError = lift . throwError
    catchError (HPT a) h = HPT $ a `catchError` (\e -> state $ h e)

type HPage = HPageT IO

evalHPage :: HPage a -> IO a
evalHPage hpt = (state hpt) `evalStateT` ""

clearPage :: HPage ()
clearPage = return ()

openPage :: FilePath -> HPage ()
openPage _ = return ()

savePage :: FilePath -> HPage ()
savePage _ = return ()

currentPage :: HPage (Maybe FilePath)
currentPage = return Nothing 

setText :: String -> HPage ()
setText s = put s

getText :: HPage String
getText = get

getExprIndex :: HPage Int
getExprIndex = return 0

setExprIndex :: Int -> HPage ()
setExprIndex _ = return () 

getExpr :: HPage String
getExpr = getText

setExpr :: String -> HPage ()
setExpr = setText

getNth :: Int -> HPage String
getNth _ = getText 

setNth :: Int -> String -> HPage ()
setNth _ = setText
                        
addExpr :: String -> HPage ()
addExpr _ = return ()

removeExpr :: HPage ()
removeExpr = setText ""

removeNth :: Int -> HPage ()
removeNth i = setNth i ""

undo, redo :: HPage ()
undo = return ()
redo = return ()

find :: String -> HPage ()
find _ = return ()

findNext :: HPage ()
findNext = return ()

type InterpreterError = ()

eval, kindOf, typeOf :: HPage (Either InterpreterError String)
eval = return $ Right "eval"
kindOf = return $ Right "kind"
typeOf = return $ Right "type"

evalNth, kindOfNth, typeOfNth :: Int -> HPage (Either InterpreterError String)
evalNth _ = return $ Right "evalNth"
kindOfNth _ = return $ Right "kindNth"
typeOfNth _ = return $ Right "typeNth"

loadModule :: FilePath -> HPage (Either InterpreterError ())
loadModule _ = return $ Right ()

reloadModules :: HPage (Either InterpreterError ())
reloadModules = return $ Right ()

reset :: HPage (Either InterpreterError ())
reset = return $ Right ()

eval', kindOf', typeOf' :: HPage (MVar (Either InterpreterError String))
eval' = liftIO $ newMVar $ Right "eval'"
kindOf' = liftIO $ newMVar $ Right "kind'"
typeOf' = liftIO $ newMVar $ Right "type'"

evalNth', kindOfNth', typeOfNth' :: Int -> HPage (MVar (Either InterpreterError String))
evalNth' _ = liftIO $ newMVar $ Right "evalNth'"
kindOfNth' _ = liftIO $ newMVar $ Right "kindNth'"
typeOfNth' _ = liftIO $ newMVar $ Right "typeNth'"

loadModule' :: FilePath -> HPage (MVar (Either InterpreterError ()))
loadModule' _ = liftIO $ newMVar $ Right ()
                            

reloadModules' :: HPage (MVar (Either InterpreterError ()))
reloadModules' = liftIO $ newMVar $ Right ()

reset' :: HPage (MVar (Either InterpreterError ()))
reset' = liftIO $ newMVar $ Right ()

cancel :: HPage ()
cancel = return ()
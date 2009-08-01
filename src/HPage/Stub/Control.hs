
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
    getPageIndex, setPageIndex, getPageCount,
    addPage, openPage, closePage, getPagePath,
    savePage, savePageAs,
    closePageNth, getPageNthPath,
    savePageNth, savePageNthAs,
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

getPageIndex :: HPage Int
getPageIndex = return (-1)

setPageIndex :: Int -> HPage ()
setPageIndex _ = return ()

getPageCount :: HPage Int
getPageCount = return (-1)

addPage :: HPage ()
addPage = return ()

closePage :: HPage ()
closePage = return ()

savePage :: HPage ()
savePage = return ()

closePageNth :: Int -> HPage ()
closePageNth _ = return ()

getPageNthPath :: Int -> HPage (Maybe FilePath)
getPageNthPath _ = return Nothing

savePageNth :: Int -> HPage ()
savePageNth _ = return ()

savePageNthAs :: Int -> FilePath -> HPage ()
savePageNthAs _ _ = return ()

clearPage :: HPage ()
clearPage = return ()

openPage :: FilePath -> HPage ()
openPage _ = return ()

savePageAs :: FilePath -> HPage ()
savePageAs _ = return ()

getPagePath :: HPage (Maybe FilePath)
getPagePath = return Nothing 

setPageText :: String -> HPage ()
setPageText s = put s

getPageText :: HPage String
getPageText = get

getExprIndex :: HPage Int
getExprIndex = return 0

setExprIndex :: Int -> HPage ()
setExprIndex _ = return () 

getExprCount :: HPage Int
getExprCount = return (-1)

getExprText :: HPage String
getExprText = getPageText

setExprText :: String -> HPage ()
setExprText = setPageText

getExprNthText :: Int -> HPage String
getExprNthText _ = getPageText 

setExprNthText :: Int -> String -> HPage ()
setExprNthText _ = setPageText
                        
addExpr :: String -> HPage ()
addExpr _ = return ()

removeExpr :: HPage ()
removeExpr = setPageText ""

removeNth :: Int -> HPage ()
removeNth i = setExprNthText i ""

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
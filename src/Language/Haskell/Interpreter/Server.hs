{-# LANGUAGE NoMonomorphismRestriction #-} 

module Language.Haskell.Interpreter.Server (
    spawn, start, runIn, asyncRunIn, ServerHandle
    ) where

import Control.Concurrent.MVar
import Control.Monad.Error
import Control.Monad.State
import Control.Concurrent.Process hiding (spawn)
import qualified Control.Concurrent.Process as P (spawn)
import Language.Haskell.Interpreter

type ServerHandle = Handle (InterpreterT IO ())

spawn :: IO (Handle (InterpreterT IO a))
spawn = P.spawn start

start :: Process (InterpreterT IO a) (Either InterpreterError ())
start = makeProcess runInterpreter interpreter
    where interpreter =
            do
                setImports ["Prelude"]
                forever $ recv >>= lift

asyncRunIn :: ServerHandle -> InterpreterT IO a -> IO (MVar (Either InterpreterError a))
asyncRunIn server action = do
                                resVar <- liftIO newEmptyMVar
                                sendTo server (try action >>= liftIO . putMVar resVar)
                                return resVar
    where try a = (a >>= return . Right) `catchError` (return . Left)

runIn :: ServerHandle -> InterpreterT IO a -> IO (Either InterpreterError a)
runIn server action = readMVar =<< asyncRunIn server action
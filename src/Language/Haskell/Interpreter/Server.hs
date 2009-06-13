
module Language.Haskell.Interpreter.Server (
    start, runIn, asyncRunIn, ServerHandle
    ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.State
import Control.Concurrent.Process
import Language.Haskell.Interpreter

type ServerHandle = Handle (InterpreterT IO ())

start :: Process (InterpreterT IO a) (Either InterpreterError ())
start = makeProcess runInterpreter interpreter
    where interpreter =
            do
                setImports ["Prelude"]
                forever $ recv >>= lift

asyncRunIn :: ServerHandle -> InterpreterT IO a -> IO (MVar a)
asyncRunIn server action = do
                                resVar <- liftIO newEmptyMVar
                                sendTo server (action >>= liftIO . putMVar resVar)
                                return resVar

runIn :: ServerHandle -> InterpreterT IO a -> IO a
runIn server action = readMVar =<< asyncRunIn server action
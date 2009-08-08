
module Language.Haskell.Interpreter.Server (
    start, stop, runIn, asyncRunIn, flush, ServerHandle
    ) where

import Control.Concurrent.MVar
import Control.Monad.Error
import Control.Monad.Loops
import Control.Concurrent.Process
import Language.Haskell.Interpreter

newtype ServerHandle = SH {handle :: Handle (Either Stop (InterpreterT IO ()))}

data Stop = Stop

instance MonadInterpreter m => MonadInterpreter (ReceiverT r m) where
    fromSession = lift . fromSession
    modifySessionRef a = lift . (modifySessionRef a)
    runGhc = lift . runGhc 

start :: IO ServerHandle
start = (spawn $ makeProcess runInterpreter interpreter) >>= return . SH
    where interpreter =
            do
                setImports ["Prelude"]
                iterateWhile id $ do
                                    v <- recv
                                    case v of
                                        Left Stop ->
                                            return False
                                        Right acc ->
                                            lift acc >> return True

asyncRunIn :: ServerHandle -> InterpreterT IO a -> IO (MVar (Either InterpreterError a))
asyncRunIn server action = do
                                resVar <- liftIO newEmptyMVar
                                sendTo (handle server) $ Right $ try action >>= liftIO . putMVar resVar
                                return resVar

runIn :: ServerHandle -> InterpreterT IO a -> IO (Either InterpreterError a)
runIn server action = runHere $ do
                                    me <- self
                                    sendTo (handle server) $ Right $ try action >>= sendTo me
                                    recv

flush :: ServerHandle -> IO (Either InterpreterError ())
flush server = runIn server $ return ()

try :: InterpreterT IO b -> InterpreterT IO (Either InterpreterError b)
try a = (a >>= return . Right) `catchError` (return . Left)

stop :: ServerHandle -> IO ()
stop server = sendTo (handle server) $ Left Stop
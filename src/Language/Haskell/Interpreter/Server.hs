
module Language.Haskell.Interpreter.Server (
    start, Command(..)
    ) where

import Control.Concurrent.MVar
import Control.Concurrent.Process hiding (init)
import qualified Control.Concurrent.Process as P (init)
import Control.Monad.State
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Server.Command

start :: Process Command (Either InterpreterError ())
start = P.init runInterpreter interpreter
    where interpreter =
            do
                setImports ["Prelude"]
                forever $ do
                            cmd <- recv
                            liftIO $ putStrLn $ ">> " ++ (show cmd)
                            res <- runCmd cmd
                            liftIO $ putStrLn $ "<< " ++ (show res)

data HPageState = Nada

type HPage = InterpreterT (StateT HPageState IO)

type HPageServer = ReceiverT (HPage ()) HPage ()

startHPage :: Process (HPage ()) (Either InterpreterError ())
startHPage = P.init ((`evalStateT` Nada). runInterpreter) action
    where action =
             do setImports ["Prelude"]
                forever $ do cmd <- recv
                             lift $ cmd

type PageHandle = Handle (HPage ())

runPage :: MonadIO m => PageHandle -> (HPage a) -> m (MVar a)
runPage h action = do res <- liftIO newEmptyMVar
                      sendTo h (action >>= liftIO . putMVar res)
                      return res
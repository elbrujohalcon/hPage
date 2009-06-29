
module HPage.Server (
    start, runIn, asyncRunIn, ServerHandle, cancel
    ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.Process
import HPage.Control

newtype ServerHandle = SH {handle :: Handle (HPage ())}

start :: IO ServerHandle
start = (spawn $ makeProcess evalHPage pageRunner) >>= return . SH
    where pageRunner = forever $ recv >>= lift

asyncRunIn :: ServerHandle -> HPage a -> IO (MVar a)
asyncRunIn server action = do
                                resVar <- liftIO newEmptyMVar
                                sendTo (handle server) (action >>= liftIO . putMVar resVar)
                                return resVar

runIn :: ServerHandle -> HPage a -> IO a
runIn server action = readMVar =<< asyncRunIn server action

cancel :: ServerHandle -> IO a
cancel = undefined
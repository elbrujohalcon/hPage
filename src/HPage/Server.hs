
module HPage.Server (
    start, stop, runIn, ServerHandle
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.Process
import HPage.Control

newtype ServerHandle = SH {handle :: Handle (HPage ())}

start :: IO ServerHandle
start = (spawn $ makeProcess evalHPage pageRunner) >>= return . SH
    where pageRunner = forever $ recv >>= lift

runIn :: ServerHandle -> HPage a -> IO a
runIn server action = runHere $ do
                                    me <- self
                                    sendTo (handle server) $ action >>= sendTo me
                                    recv

stop :: ServerHandle -> IO ()
stop = kill . handle
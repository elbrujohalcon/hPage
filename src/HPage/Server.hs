
module HPage.Server (
    start, stop, runIn, ServerHandle
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Loops
import Control.Concurrent.Process
import HPage.Control
import HPage.Utils.Log

newtype ServerHandle = SH {handle :: Handle (Either Stop (HPage ()))}

data Stop = Stop

start :: IO ServerHandle
start = (spawn $ makeProcess evalHPage pageRunner) >>= return . SH
    where pageRunner = iterateWhile id $ do
                                            liftTraceIO "hps iterating..."
                                            v <- recv
                                            case v of
                                                Left Stop ->
                                                    do
                                                        liftTraceIO "hps stop"
                                                        return False
                                                Right acc ->
                                                    do
                                                        liftTraceIO "hps continue"
                                                        lift acc >> return True

runIn :: ServerHandle -> HPage a -> IO a
runIn server action = runHere $ do
                                    me <- self
                                    sendTo (handle server) $ Right $ action >>= sendTo me
                                    recv

stop :: ServerHandle -> IO ()
stop server = sendTo (handle server) $ Left Stop

module HPage.IOServer (
    start, stop, runIn, ServerHandle
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.Process

newtype ServerHandle = SH {handle :: Handle (IO ())}

start :: IO ServerHandle
start = spawn ioRunner >>= return . SH
    where ioRunner = forever $ recv >>= liftIO

runIn :: ServerHandle -> IO a -> IO a
runIn server action = runHere $ do
                                    me <- self
                                    sendTo (handle server) $ action >>= sendTo me
                                    recv

stop :: ServerHandle -> IO ()
stop = kill . handle
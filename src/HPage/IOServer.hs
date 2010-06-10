
module HPage.IOServer (
    start, stop, runIn, ServerHandle
    ) where

import Control.Exception (try, SomeException)
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.Process
import Control.Concurrent.MVar

newtype ServerHandle = SH {handle :: Handle (IO ())}

start :: IO ServerHandle
start = spawn ioRunner >>= return . SH
    where ioRunner = forever $ recv >>= liftIO

runIn :: ServerHandle -> IO a -> IO (MVar (Either SomeException a))
runIn server action = runHere $ do
                                    var <- liftIO $ newEmptyMVar
                                    sendTo (handle server) $ try action >>= putMVar var
                                    return var

stop :: ServerHandle -> IO ()
stop = kill . handle
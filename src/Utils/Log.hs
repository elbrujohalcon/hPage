
module Utils.Log where

import Control.Monad.Trans

data LogLevel = Trace | Debug | Info | Warning | Error | Fatal
    deriving (Show, Eq)

logIO :: Show a => LogLevel -> a -> IO ()
logIO lvl msg = putStrLn $ (show lvl) ++ ": " ++ (show msg)

liftLogIO :: (MonadIO m, Show a) => LogLevel -> a -> m ()
liftLogIO lvl = liftIO . (logIO lvl) 

traceIO, debugIO, infoIO, warnIO, errorIO, fatalIO :: Show a => a -> IO ()
liftTraceIO, liftDebugIO, liftInfoIO, liftWarnIO, liftErrorIO, liftFatalIO :: (MonadIO m, Show a) => a -> m ()

traceIO = logIO Trace
debugIO = logIO Debug
{- without log...
traceIO _ = return ()
debugIO _ = return ()
-}
infoIO = logIO Info
warnIO = logIO Warning
errorIO = logIO Error
fatalIO = logIO Fatal

{- with log...
liftTraceIO = liftLogIO Trace
-}
liftTraceIO _ = return ()
liftDebugIO = liftLogIO Debug
liftInfoIO = liftLogIO Info
liftWarnIO = liftLogIO Warning
-- liftErrorIO = liftLogIO Error
liftErrorIO _ = return ()
liftFatalIO = liftLogIO Fatal
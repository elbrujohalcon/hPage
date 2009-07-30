{-# LANGUAGE GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances,
             FunctionalDependencies,
             UndecidableInstances #-} 

module Control.Concurrent.Process (
        Handle, sendTo,
        ReceiverT, recv, self,
        sendRecv,
        Process, makeProcess, runHere, spawn
    ) where

import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Error.Class
import Control.Monad.CatchIO
import Data.Monoid
import Control.Concurrent
import Control.Concurrent.Chan

newtype Handle r = PH {chan :: Chan r}

newtype ReceiverT r m a = RT { internalReader :: ReaderT (Handle r) m a }
    deriving (Monad, MonadIO, MonadTrans, MonadCatchIO)

type Process r = ReceiverT r IO

sendTo :: MonadIO m => Handle a -> a -> m ()
sendTo ph = liftIO . writeChan (chan ph)

recv :: MonadIO m => ReceiverT r m r
recv = RT $ ask >>= liftIO . readChan . chan

sendRecv :: MonadIO m => Handle a -> a -> ReceiverT r m r
sendRecv h a = sendTo h a >> recv 

spawn :: MonadIO m => Process r k -> m (Handle r)  
spawn p = liftIO $ do
                 pChan <- newChan
                 let handle = PH { chan = pChan }
                 let action = runReaderT (internalReader p) handle
                 forkIO $ action >> return ()
                 return handle

runHere :: MonadIO m => Process r t -> m t
runHere p = liftIO (runReaderT (internalReader p) . PH =<< newChan)

self :: Monad m => ReceiverT r m (Handle r)
self = RT ask

makeProcess :: (m t -> IO s) -> ReceiverT r m t -> Process r s 
makeProcess f (RT a) = RT (mapReaderT f a)

instance MonadState s m => MonadState s (ReceiverT r m) where
    get = lift get
    put = lift . put

instance MonadReader r m => MonadReader r (ReceiverT r m) where
    ask = lift ask
    local = onInner . local 

instance (Monoid w, MonadWriter w m) => MonadWriter w (ReceiverT w m) where
    tell = lift . tell
    listen = onInner listen
    pass = onInner pass

instance MonadError e m => MonadError e (ReceiverT r m) where
    throwError = lift . throwError
    catchError (RT a) h = RT $ a `catchError` (\e -> internalReader $ h e)

onInner :: (m a -> m b) -> ReceiverT r m a -> ReceiverT r m b
onInner f (RT m) = RT $ mapReaderT f m

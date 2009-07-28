
module HPage.Stub.Server (
    start, runIn, ServerHandle
    ) where

import Control.Monad
import HPage.Stub.Control

newtype ServerHandle = SH String

start :: IO ServerHandle
start = return $ SH "El Server"

runIn :: ServerHandle -> HPage a -> IO a
runIn _ action = evalHPage action
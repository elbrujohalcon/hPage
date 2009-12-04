module FailTest where

data WithBottom = WithBottom
data WithBottomString = WithBottomString
data InfiniteString = InfiniteString

instance Show WithBottom where
    show _ = ['w', 'i', 't', 'h', ' ', undefined] ++ " in the name"
    
instance Show WithBottomString where
    show _ = "with bottom string" ++ (':': undefined)
    
instance Show InfiniteString where
    show _ = show [1..]
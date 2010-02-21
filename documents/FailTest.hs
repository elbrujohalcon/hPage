module FailTest where

data WithBottom = WithBottom
data WithBottomString = WithBottomString
data InfiniteString = InfiniteString
data InfiniteChar = InfiniteChar

instance Show WithBottom where
    show _ = ['w', 'i', 't', 'h', ' ', undefined] ++ " in the name"
    
instance Show WithBottomString where
    show _ = "with bottom string" ++ (':': undefined)
    
instance Show InfiniteString where
    show _ = show [1..]
    
instance Show InfiniteChar where
    show _ = ['w', 'i', 't', 'h', ' ', head . show $ length [1..]] ++ " which yields an infinite calculation"

data WithIfiniteChar = WIC

instance Show WithIfiniteChar where
    show WIC = ['c', head . show $ length [1..]]
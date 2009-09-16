class ExtTest a where
    extes :: a -> String
    
instance ExtTest String where
    extes = show . map show
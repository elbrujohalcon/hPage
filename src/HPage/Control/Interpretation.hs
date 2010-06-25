{-# LANGUAGE FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, TypeSynonymInstances #-}

module HPage.Control.Interpretation where

import Data.Typeable

data Interpretation = KindInt   {intKind   :: String} |
                      TypeInt   {intType   :: String} |
                      ValueInt  {intValue  :: Presentation, intType :: String}
    deriving (Typeable)

instance Show Interpretation where
    show (KindInt x) = "type :: " ++ show x
    show (TypeInt x) = "value :: " ++ show x
    show (ValueInt p x) = show p ++ " :: " ++ show x
    
data Presentation = StringPres String | IOPres (IO Presentation) | ListPres [Presentation]
    deriving (Typeable)

instance Show Presentation where
    show (StringPres s) = s
    show (IOPres _)     = "ioAction"
    show (ListPres l)   = show l
    
class Presentable a where
    present :: a -> Presentation
    
instance Presentable a => Presentable (IO a) where
    present x = IOPres $ x >>= return . present
    
instance Presentable a => Presentable [a] where
    present = ListPres . map present

instance Presentable String where
    present = StringPres . id

instance Show a => Presentable a where
    present = StringPres . show
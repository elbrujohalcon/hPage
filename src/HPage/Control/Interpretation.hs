
module HPage.Control.Interpretation where

import Control.Concurrent.MVar
import Control.Exception (SomeException)

data Interpretation = Type  {intKind   :: String} |
                      Expr  {intValue  :: String,   intType :: String} |
                      IOExpr{intResult :: MVar (Either SomeException String), intType :: String} |
                      Exprs {intValues :: [String], intType :: String} 
    deriving (Eq)
instance Show Interpretation where
    show (Type x) = "type :: " ++ show x
    show (IOExpr _ t) = " io :: " ++ show t
    show (Expr x t) = x ++ show t
    show (Exprs xs t) = show xs ++ " :: [" ++ show t ++ "]"
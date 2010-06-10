
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
    
isIntType :: Interpretation -> Bool
isIntType Type{}  = True
isIntType IOExpr{}= False
isIntType Expr{}  = False
isIntType Exprs{} = False

isIntExprs :: Interpretation -> Bool
isIntExprs Type{}  = False
isIntExprs IOExpr{}= False
isIntExprs Expr{}  = False
isIntExprs Exprs{} = True

isIntExpr :: Interpretation -> Bool
isIntExpr Type{}  = False
isIntExpr IOExpr{}= False
isIntExpr Expr{}  = True
isIntExpr Exprs{} = False

isIntIOExpr :: Interpretation -> Bool
isIntIOExpr Type{}  = False
isIntIOExpr IOExpr{}= True
isIntIOExpr Expr{}  = False
isIntIOExpr Exprs{} = False
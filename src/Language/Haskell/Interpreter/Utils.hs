
module Language.Haskell.Interpreter.Utils (prettyPrintError) where

import Language.Haskell.Interpreter

prettyPrintError :: InterpreterError -> String
prettyPrintError (WontCompile ghcerr)  = "Can't compile: " ++ (joinWith "\n" $ map errMsg ghcerr)
prettyPrintError (UnknownError errStr) = "Unknown Error: " ++ errStr
prettyPrintError (NotAllowed errStr)   = "Not Allowed Action: " ++ errStr
prettyPrintError (GhcException errStr) = errStr

joinWith :: [a] -> [[a]] -> [a]
joinWith _ [] = []
joinWith sep (x:xs) = x ++ (concat . map (sep ++) $ xs)

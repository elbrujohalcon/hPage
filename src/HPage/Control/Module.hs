
module HPage.Control.Module where

import HPage.Utils

data ModuleDescription = ModDesc {modName :: String,
                                  modInterpreted :: Bool}
    deriving (Eq)

instance Show ModuleDescription where
    show m = show (modName m, modInterpreted m)

data ModuleElemDesc = MEFun {funName :: String,
                             funType :: String} |
                      MEClass {clsName :: String,
                               clsFuns :: [ModuleElemDesc]} |
                      MEData {datName :: String,
                              datCtors :: [ModuleElemDesc]}
    deriving (Eq)

instance Show ModuleElemDesc where
    show MEFun{funName = fn, funType = []} = fn
    show MEFun{funName = fn, funType = ft} = fn ++ " :: " ++ ft
    show MEClass{clsName = cn, clsFuns = []} = "class " ++ cn
    show MEClass{clsName = cn, clsFuns = cfs} = "class " ++ cn ++ " where " ++ joinWith "\n" (map show cfs)
    show MEData{datName = dn, datCtors = []} = "data " ++ dn
    show MEData{datName = dn, datCtors = dcs} = "data " ++ dn ++ " = " ++ joinWith " | " (map show dcs)
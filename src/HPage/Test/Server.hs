module HPage.Test.Server where

import Data.Char
import GHC.IOBase
import Control.Monad.Error
import Test.QuickCheck
import Test.QuickCheck.Batch
import qualified HPage.Control as HP
import qualified HPage.Server as HPS
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Server as HS
import System.Directory
import Control.Monad.Loops
import qualified Data.ByteString.Char8 as Str
import HPage.Utils.Log
import Paths_hpage

instance Arbitrary Char where
    arbitrary = elements (['A'..'Z'] ++ ['a' .. 'z'])
    coarbitrary c = variant (ord c `rem` 16)

newtype ModuleName = MN {mnString :: String}
    deriving (Eq)

instance Show ModuleName where
    show = mnString

instance Arbitrary ModuleName where
    arbitrary = do
                    s <- arbitrary
                    return . MN $ "Test" ++ map toLower s
    coarbitrary _ = undefined

newtype KnownModuleName = KMN {kmnString :: String}
    deriving (Eq, Show)

instance Arbitrary KnownModuleName where
    arbitrary = elements $ map KMN ["Data.List", "Control.Monad", "System.Directory", "Control.Monad.Loops"]
    coarbitrary _ = undefined
    

newtype ClassName = CN {cnString :: String}
    deriving (Eq, Show)

instance Arbitrary ClassName where
    arbitrary = elements $ map CN ["HPage", "IO", "IO a", "Int", "String"]
    coarbitrary _ = undefined


instance Arbitrary HP.Extension where
    arbitrary = elements HP.availableExtensions
    coarbitrary _ = undefined


data WorkingExtension = WEX {wexExts :: [HP.Extension],
                             wexModule :: String}
    deriving (Eq, Show)
    
instance Arbitrary WorkingExtension where
    arbitrary = elements [wexTypeSynonymInstances, wexOverlappingInstances, wexFlexibleInstances]
    coarbitrary _ = undefined

wexTypeSynonymInstances :: WorkingExtension
wexTypeSynonymInstances = WEX [HP.TypeSynonymInstances] "TypeSynonymInstances.hs"

wexOverlappingInstances :: WorkingExtension
wexOverlappingInstances = WEX [HP.TypeSynonymInstances,
                               HP.OverlappingInstances] "OverlappingInstances.hs"

wexFlexibleInstances :: WorkingExtension
wexFlexibleInstances = WEX [HP.FlexibleInstances] "FlexibleInstances.hs"

shouldFail :: (MonadError e m) => m a -> m Bool
shouldFail a = (a >> return False) `catchError` (\_ -> return True)

options :: TestOptions
options = TestOptions
      { no_of_tests         = 200
      , length_of_tests     = 1
      , debug_tests         = False }

testDir :: FilePath
testDir = "TestFiles"

main :: IO ()
main =
    do
        createDirectoryIfMissing True testDir
        hps <- HPS.start
        hs <- HS.start
        runTests "Editing" options
                 [  run $ prop_setget_text hps
                 ,  run $ prop_setget_expr hps
                 ,  run $ prop_setget_expr_fail hps
                 ,  run $ prop_addrem_expr hps
                 ,  run $ prop_addrem_expr_fail hps
                 ,  run $ prop_setget_nth hps
                 ,  run $ prop_setget_nth_fail hps
                 ,  run $ prop_remove_nth hps
                 ,  run $ prop_remove_nth_fail hps
                 ,  run $ prop_undoredo hps
                 ]
        runTests "Many Pages" options
                 [  run $ prop_new_page hps
                 ,  run $ prop_open_page hps
                 ,  run $ prop_open_page_fail hps
                 ,  run $ prop_setget_page hps
                 ,  run $ prop_set_page_index_fail hps
                 ,  run $ prop_save_page hps
                 ,  run $ prop_save_page_fail hps
                 ,  run $ prop_save_page_as hps
                 ,  run $ prop_close_page hps
                 ,  run $ prop_is_modified_page hps
                 ,  run $ prop_save_nth_page hps
                 ,  run $ prop_save_nth_page_fail hps
                 ,  run $ prop_save_nth_page_as hps
                 ,  run $ prop_save_nth_page_as_fail hps
                 ,  run $ prop_is_modified_nth_page hps
                 ,  run $ prop_is_modified_nth_page_fail hps
                 ,  run $ prop_close_nth_page hps
                 ,  run $ prop_close_nth_page_fail hps
                 ,  run $ prop_close_all_pages hps
                 ]
        runTests "Many Pages (Safe)" options
                 [  run $ prop_safe_save_page_as hps
                 ,  run $ prop_safe_close_page hps
                 ,  run $ prop_safe_save_nth_page_as hps
                 ,  run $ prop_safe_close_nth_page hps
                 ,  run $ prop_safe_close_all_pages hps
                 ]
        runTests "Named Expressions" options
                 [  run $ prop_let_fail hps hs
                 ,  run $ prop_let_valueOf hps hs
                 ,  run $ prop_let_typeOf hps hs
                 ]
        runTests "Expressions" options
                 [  run $ prop_fail hps hs
                 ,  run $ prop_valueOf hps hs
                 ,  run $ prop_typeOf hps hs
                 ,  run $ prop_kindOf hps hs
                 ,  run $ prop_load_module hps hs
                 ,  run $ prop_import_module hps
                 ,  run $ prop_reload_modules hps hs
                 ,  run $ prop_get_loaded_modules hps hs
                 ,  run $ prop_get_module_exports hps hs
                 ]
        runTests "Cancelation" options
                 [  run $ prop_sequential hps
                 ,  run $ prop_cancel_load hps
                 ]
        runTests "Extensions" options
                 [  run prop_get_available_extensions
                 ,  run $ prop_get_set_extensions hps
                 ,  run $ prop_working_extensions hps
                 ,  run $ prop_get_set_extension_fail hps
                 ]
        runTests "Src. Dirs." options
                 [  run $ prop_get_set_source_dirs hps
                 ,  run $ prop_working_source_dirs hps
                 ]
        runTests "GHC Options" options
                 [  run $ prop_get_set_ghc_opts hps
                 ,  run $ prop_get_set_ghc_opts_fail hps
                 ,  run $ prop_working_ghc_opts hps
                 ]
        removeDirectoryRecursive testDir
                    
instance Eq (Hint.InterpreterError) where
    a == b = show a == show b

prop_valueOf :: HPS.ServerHandle -> HS.ServerHandle -> String -> Bool
prop_valueOf hps hs txt =
    unsafePerformIO $ do
                        let expr = "length \"" ++ txt ++ "\"" 
                        hpsr <- HPS.runIn hps $ HP.setPageText expr 0 >> HP.valueOf
                        hsr <- HS.runIn hs $ Hint.eval expr
                        return $ hpsr == hsr

prop_typeOf :: HPS.ServerHandle -> HS.ServerHandle -> String -> Property
prop_typeOf hps hs txt = txt /= "" ==>
    unsafePerformIO $ do
                        let h = head txt
                        let expr = if isNumber h then [h, h] else "\"" ++ txt ++ "\""
                        hpsr <- HPS.runIn hps $ HP.setPageText expr 0 >> HP.typeOf
                        hsr <- HS.runIn hs $ Hint.typeOf expr
                        return $ hpsr == hsr

prop_kindOf :: HPS.ServerHandle -> HS.ServerHandle -> ClassName -> Bool
prop_kindOf hps hs (CN expr) =
    unsafePerformIO $ do
                        hpsr <- HPS.runIn hps $ HP.setPageText expr 0 >> HP.kindOf
                        hsr <- HS.runIn hs $ Hint.kindOf expr
                        return $ hpsr == hsr

prop_fail :: HPS.ServerHandle -> HS.ServerHandle -> String -> Bool
prop_fail hps hs txt =
    unsafePerformIO $ do
                        let expr = "lenggth \"" ++ txt ++ "\""
                        Left hpsr <- HPS.runIn hps $ HP.setPageText expr 0 >> HP.valueOf
                        Left hsr <- HS.runIn hs $ Hint.eval expr
                        return $ hsr == hpsr
    
prop_import_module :: HPS.ServerHandle -> KnownModuleName -> Bool
prop_import_module hps kmn =
        unsafePerformIO $ do
                            let mn = kmnString kmn
                            HPS.runIn hps $ do
                                                r1 <- HP.importModules [mn]
                                                r2 <- HP.getImportedModules
                                                r3 <- HP.importModules [mn]
                                                r4 <- HP.getImportedModules
                                                return $ r1 == (Right ()) &&
                                                         r3 == (Right ()) &&
                                                         mn `elem` r2 &&
                                                         mn `elem` r4

prop_get_module_exports :: HPS.ServerHandle -> HS.ServerHandle -> KnownModuleName -> Bool
prop_get_module_exports hps hs kmn =
        unsafePerformIO $ do
                            let mn = kmnString kmn
                            Right hpsr <- HPS.runIn hps $ HP.importModules [mn] >> HP.getModuleExports mn
                            Right hsr  <- HS.runIn hs $ Hint.setImports ["Prelude", mn] >> Hint.getModuleExports mn
                            -- liftDebugIO (hpsr, hsr)
                            return $ all match  $ zip hpsr hsr
    where match ((HP.MEFun fn _), (Hint.Fun fn2)) = fn == fn2
          match ((HP.MEClass cn cfs), (Hint.Class cn2 cfs2)) = cn == cn2 && all match (zip cfs (map Hint.Fun cfs2))
          match ((HP.MEData dn dcs), (Hint.Data dn2 dcs2)) = dn == dn2 && all match (zip dcs (map Hint.Fun dcs2))

prop_load_module :: HPS.ServerHandle -> HS.ServerHandle -> ModuleName -> Property
prop_load_module hps hs mn =
    (show mn /= "Test") ==>
        unsafePerformIO $ do
                            let mname = testDir ++ "." ++ show mn
                            let ftxt = "module " ++ mname ++ " where v = 32"
                            let f2txt = "v = \"" ++ show mn ++ "\""
                            hpsr <- HPS.runIn hps $ do
                                                        -- Save TestFiles/Test...hs
                                                        HP.setPageText ftxt 0
                                                        HP.savePageAs $ testDir ++ "/" ++ show mn ++ ".hs"
                                                        -- Save TestFiles/test.hs
                                                        HP.setPageText f2txt 0
                                                        HP.savePageAs $ testDir ++ "/test.hs"
                                                        -- Load TestFiles/test.hs by path
                                                        HP.loadModules [testDir ++ "/test.hs"]
                                                        HP.setPageText "v" 0
                                                        fv <- HP.valueOf
                                                        fm <- HP.getLoadedModules >>= return . mN
                                                        -- Load TestFiles/Test...hs by name
                                                        HP.loadModules [mname]
                                                        HP.setPageText "v" 0
                                                        sv <- HP.valueOf
                                                        sm <- HP.getLoadedModules >>= return . mN
                                                        return (fv, sv, fm, sm)
                            hsr <- HS.runIn hs $ do
                                                    Hint.loadModules [testDir ++ "/test.hs"]
                                                    Hint.getLoadedModules >>= Hint.setTopLevelModules
                                                    fv <- Hint.eval "v"
                                                    fm <- Hint.getLoadedModules
                                                    Hint.loadModules [mname]
                                                    Hint.getLoadedModules >>= Hint.setTopLevelModules
                                                    sv <- Hint.eval "v"
                                                    sm <- Hint.getLoadedModules
                                                    return (Right fv, Right sv, Right fm, Right sm)
                            -- liftDebugIO (hpsr, hsr)
                            return $ Right hpsr == hsr
        where mN (Right mns) = Right $ map HP.modName mns
              mN (Left err) = Left err
                            

prop_reload_modules :: HPS.ServerHandle -> HS.ServerHandle -> String -> Bool
prop_reload_modules hps hs txt =
    unsafePerformIO $ do
                        let expr = "test = show \"" ++ txt ++ "\"" 
                        hpsr <- HPS.runIn hps $ do
                                                    HP.setPageText expr 0
                                                    HP.savePageAs $ testDir ++ "/test.hs"
                                                    HP.setPageText "test" 0
                                                    HP.loadModules [testDir ++ "/test.hs"]
                                                    HP.reloadModules
                                                    HP.valueOf
                        hsr <- HS.runIn hs $ do
                                                Hint.loadModules [testDir ++ "/test.hs"]
                                                Hint.getLoadedModules >>= Hint.setTopLevelModules
                                                Hint.eval "test"
                        return $ hpsr == hsr
    
prop_get_loaded_modules :: HPS.ServerHandle -> HS.ServerHandle -> ModuleName -> Bool
prop_get_loaded_modules hps hs mn =
    unsafePerformIO $ do
                        let expr1 = "ytest = show \"" ++ show mn ++ "\""
                        let expr2 = "module " ++ testDir ++ ".XX" ++ show mn ++ "2 where import " ++ testDir ++ ".XX" ++ show mn ++ "3; xtest = show \"" ++ show mn ++ "\""
                        let expr3 = "module " ++ testDir ++ ".XX" ++ show mn ++ "3 where xfact = (1,2,3)"
                        let mnf1  = testDir ++ "/XX" ++ (show mn) ++ "1.hs"
                        let mnf2  = testDir ++ "/XX" ++ (show mn) ++ "2.hs"
                        let mnf3  = testDir ++ "/XX" ++ (show mn) ++ "3.hs"
                        HPS.runIn hps $ do
                                            HP.setPageText expr1 0
                                            HP.savePageAs mnf1
                                            HP.setPageText expr2 0
                                            HP.savePageAs mnf2
                                            HP.setPageText expr3 0
                                            HP.savePageAs mnf3
                        hpsr1 <- HPS.runIn hps $ HP.loadModules [mnf1] >> HP.getLoadedModules
                        hsr1 <- HS.runIn hs $ Hint.loadModules [mnf1] >> Hint.getLoadedModules
                        hpsr2 <- HPS.runIn hps $ HP.loadModules [mnf2] >> HP.getLoadedModules
                        hsr2 <- HS.runIn hs $ Hint.loadModules [mnf2] >> Hint.getLoadedModules
                        hpsr3 <- HPS.runIn hps $ HP.loadModules [mnf3] >> HP.getLoadedModules
                        hsr3 <- HS.runIn hs $ Hint.loadModules [mnf3] >> Hint.getLoadedModules
                        --liftDebugIO [(hpsr1, hpsr2, hpsr3), (hsr1, hsr2, hsr3)]
                        return $ (mN hpsr1, mN hpsr2, mN hpsr3) == (hsr1, hsr2, hsr3)
        where mN (Right mns) = Right $ map HP.modName mns
              mN (Left err) = Left err

prop_sequential :: HPS.ServerHandle -> String -> Bool
prop_sequential hps txt =
    unsafePerformIO $ do
                        let expr = "test = \"" ++ txt ++ "\""
                        HPS.runIn hps $ do
                                            HP.setPageText expr 0
                                            HP.savePageAs $ testDir ++ "/test.hs"
                                            HP.loadModules' [testDir ++ "/test.hs"]
                        Right hpsr <- HPS.runIn hps $ do
                                                        HP.setPageText "test" 0
                                                        HP.valueOf
                        return $ hpsr == show txt

prop_cancel_load :: HPS.ServerHandle -> ModuleName -> Bool
prop_cancel_load hps mn =
    unsafePerformIO $ do
                        let expr1 = "module " ++ show mn ++ " where cancelLoadTest = (1,2,3)"
                        let expr2 = "module " ++ show mn ++ "2 where cancelLoadTest = foldl (*) 1 [1.." ++ show (length $ show mn) ++ "]"
                        HPS.runIn hps $ do
                                            HP.reset
                                            HP.setPageText expr2 0
                                            HP.savePageAs $ testDir ++ "/" ++ show mn ++ "2.hs"
                                            HP.setPageText expr1 0
                                            HP.savePageAs $ testDir ++ "/" ++ show mn ++ ".hs"
                                            HP.setPageText "cancelLoadTest" 0
                                            HP.loadModules [testDir ++ "/" ++ show mn ++ ".hs"]
                                            oldMs <- HP.getLoadedModules
                                            oldRes <- HP.valueOf
                                            oldCtx <- HP.ctxString
                                            HP.loadModules' [testDir ++ "/" ++ show mn ++ "2.hs"]
                                            HP.cancel
                                            newMs <- HP.getLoadedModules
                                            newRes <- HP.valueOf
                                            newCtx <- HP.ctxString
                                            -- liftDebugIO [(oldRes, oldMs, oldCtx), (newRes, newMs, newCtx)]
                                            return $ (oldRes, oldMs, oldCtx) == (newRes, newMs, newCtx)

prop_setget_text :: HPS.ServerHandle -> String -> Bool
prop_setget_text hps txt =
    unsafePerformIO $ HPS.runIn hps $ do
                                        HP.setPageText txt 0
                                        HP.getPageText >>= return . (txt ==)

prop_setget_expr :: HPS.ServerHandle -> String -> Property
prop_setget_expr hps txt =
    txt /= "" ==>
    unsafePerformIO $ HPS.runIn hps $ do
                                        HP.setPageText (txt ++ "\n\nxx") (length txt + 3)
                                        exi1 <- HP.getExprIndex
                                        exp1 <- HP.getExprText
                                        HP.setExprIndex 0
                                        exi0 <- HP.getExprIndex
                                        exp0 <- HP.getExprText
                                        HP.setExprText "yy"
                                        exi2 <- HP.getExprIndex
                                        exp2 <- HP.getExprText
                                        HP.setExprIndex 1
                                        exi3 <- HP.getExprIndex
                                        exp3 <- HP.getExprText
                                        -- liftDebugIO [(exi1, exp1), (exi0, exp0), (exi2, exp2), (exi3, exp3)]
                                        return $ (exi1 == 1) && (exp1 == "xx") &&
                                                 (exi0 == 0) && (exp0 == txt) &&
                                                 (exi2 == 0) && (exp2 == "yy") &&
                                                 (exi3 == 1) && (exp3 == "xx")

prop_setget_expr_fail :: HPS.ServerHandle -> String -> Bool
prop_setget_expr_fail hps _ =
    unsafePerformIO $ HPS.runIn hps $ shouldFail $ HP.clearPage >> HP.getExprText

prop_addrem_expr :: HPS.ServerHandle -> String -> Property
prop_addrem_expr hps txt =
    txt /= "" ==>
    unsafePerformIO $ HPS.runIn hps $ do
                                        HP.clearPage
                                        HP.addExpr txt
                                        exi1 <- HP.getExprIndex
                                        exp1 <- HP.getExprText
                                        HP.removeExpr
                                        exi0 <- HP.getExprIndex
                                        exp0 <- HP.getPageText
                                        HP.addExpr txt
                                        HP.addExpr txt
                                        exi2 <- HP.getExprIndex
                                        exp2 <- HP.getExprText
                                        HP.removeExpr
                                        exi3 <- HP.getExprIndex
                                        exp3 <- HP.getPageText
                                        -- liftDebugIO [(exi1, exp1), (exi0, exp0), (exi2, exp2), (exi3, exp3)]
                                        return $ (exi1 == 0) && (exp1 == txt) &&
                                                 (exi0 == -1) && (exp0 == "") &&
                                                 (exi2 == 1) && (exp2 == txt) &&
                                                 (exi3 == 0) && (exp3 == txt)

prop_addrem_expr_fail :: HPS.ServerHandle -> String -> Bool
prop_addrem_expr_fail hps _ =
    unsafePerformIO $ HPS.runIn hps $ shouldFail $ HP.clearPage >> HP.removeExpr

prop_setget_nth :: HPS.ServerHandle -> Int -> Property
prop_setget_nth hps i =
    i >= 0 ==>
    unsafePerformIO $ HPS.runIn hps $ do
                                        HP.clearPage
                                        replicateM (i+1) $ HP.addExpr "ww"
                                        exp0 <- HP.getExprNthText 0
                                        exp1 <- HP.getExprNthText i
                                        HP.setExprNthText i "xxx"
                                        exp2 <- HP.getExprNthText i
                                        HP.setExprNthText i "x\n\ny"
                                        exp3 <- HP.getExprNthText $ i+1
                                        -- liftDebugIO [exp0, exp1, exp2, exp3]
                                        return $ (exp1 == "ww") &&
                                                 (exp0 == "ww") &&
                                                 (exp2 == "xxx") &&
                                                 (exp3 == "y")

prop_setget_nth_fail :: HPS.ServerHandle -> Int -> String -> Property
prop_setget_nth_fail hps i txt =
    i > 0 ==>
    unsafePerformIO $ HPS.runIn hps $ do
                                        HP.clearPage
                                        set <- shouldFail $ HP.setExprNthText i txt
                                        get <- shouldFail $ HP.getExprNthText i
                                        -- liftDebugIO (get, set)
                                        return (set && get)

prop_remove_nth :: HPS.ServerHandle -> Int -> Property
prop_remove_nth hps i =
    i > 0 ==>
    unsafePerformIO $ HPS.runIn hps $ do
                                        HP.clearPage
                                        forM [0..i] $ HP.addExpr . show
                                        HP.removeNth 0
                                        exp0 <- HP.getExprNthText 0
                                        forM [i-2,i-3..0] $ HP.removeNth
                                        exp1 <- HP.getPageText
                                        -- liftDebugIO [exp0, exp1]
                                        return $ (exp0 == "1") &&
                                                 (exp1 == show i)

prop_remove_nth_fail :: HPS.ServerHandle -> Int -> Property
prop_remove_nth_fail hps i =
    i >= 0 ==>
    unsafePerformIO $ HPS.runIn hps $ HP.clearPage >> shouldFail (HP.removeNth $ i+1)


prop_undoredo :: HPS.ServerHandle -> String -> Property
prop_undoredo hps txt =
    txt /= "" ==>
    unsafePerformIO $ HPS.runIn hps $ do
                                        HP.addPage
                                        b0 <- HP.getPageText
                                        HP.setPageText txt 0
                                        b1 <- HP.getPageText
                                        HP.addExpr "xx"
                                        b2 <- HP.getPageText
                                        HP.addExpr "yy"
                                        b3 <- HP.getPageText
                                        HP.setExprIndex 1
                                        b4 <- HP.getPageText
                                        HP.setExprText $ txt ++ "|" ++ txt
                                        b5 <- HP.getPageText
                                        HP.removeExpr
                                        b6 <- HP.getPageText
                                        HP.removeNth 0
                                        b7 <- HP.getPageText
                                        HP.setExprNthText 0 txt
                                        b8 <- HP.getPageText
                                        HP.addExpr "zz"
                                        b9 <- HP.getPageText
                                        let to10 = [0..10] :: [Int]
                                        after <- mapM (\_ -> HP.undo >> HP.getPageText) to10
                                        redo <- mapM (\_ -> HP.redo >> HP.getPageText) to10
                                        HP.clearPage >> HP.addExpr "cc" >> HP.clearPage
                                        c0 <- HP.addExpr "zz" >> HP.getPageText
                                        c1 <- HP.undo >> HP.getPageText
                                        c2 <- HP.undo >> HP.getPageText
                                        c3 <- HP.setPageText "ww" 0 >> HP.getPageText
                                        c4 <- HP.redo >> HP.getPageText
                                        let result = ([b8, b7, b6, b5, b4, b3, b2, b1, b0, "", ""] == after) &&
                                                     ([b1, b2, b3, b4, b5, b6, b7, b8, b9, b9, b9] == redo) &&
                                                     ([c0, c1, c2, c3, c4] == ["zz", "", "cc", "ww", "ww"])
                                        if not result
                                            then
                                                do
                                                    liftDebugIO [b8, b7, b6, b5, b4, b3, b2, b1, b0, "", ""]
                                                    liftDebugIO after
                                                    liftDebugIO [b1, b2, b3, b4, b5, b6, b7, b8, b8, b8, b8]
                                                    liftDebugIO redo
                                                    liftDebugIO ["zz", "", "cc", "ww", "ww"]
                                                    liftDebugIO [c0, c1, c2, c3, c4]
                                                    return False
                                            else
                                                return True 

prop_new_page :: HPS.ServerHandle -> Int -> Property
prop_new_page hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.clearPage
                                            HP.closeAllPages
                                            pc0 <- HP.getPageCount
                                            pi0 <- HP.getPageIndex
                                            pt0 <- HP.getPageText
                                            pss <- (flip mapM) [1..i] $ \x -> do
                                                                                HP.addPage
                                                                                psc <- HP.getPageCount
                                                                                psi <- HP.getPageIndex
                                                                                pst <- HP.getPageText
                                                                                HP.setPageIndex $ psc - 1
                                                                                HP.setPageText ("old "++ show x) 0
                                                                                return (x, psc, psi, pst)
                                            let results = (0,pc0,pi0,pt0):pss
                                            -- liftDebugIO results
                                            return $ all (\(k, kc, ki, kt) ->
                                                            kc == k+1 &&
                                                            ki == 0 &&
                                                            kt == "") $ results

prop_open_page, prop_open_page_fail :: HPS.ServerHandle -> String -> Property
prop_open_page hps file =
    file /= "" ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            let path = testDir ++ "/Test" ++ file
                                            Hint.liftIO $ writeFile path file
                                            HP.closeAllPages
                                            HP.openPage path
                                            liftM (file ==) HP.getPageText

prop_open_page_fail hps file =
    file /= "" ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            let path = testDir ++ "/NO-Test" ++ file
                                            HP.closeAllPages
                                            shouldFail $ HP.openPage path

prop_setget_page, prop_set_page_index_fail :: HPS.ServerHandle -> Int -> Property
prop_setget_page hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.clearPage
                                            HP.closeAllPages
                                            HP.setPageText "0" 0
                                            forM [1..i] $ \x ->
                                                            do
                                                                HP.addPage
                                                                HP.setPageText (show x) 0 
                                            pc <- HP.getPageCount
                                            pss <- (flip mapM) [0..i] $ \x -> do
                                                                                HP.setPageIndex (i-x)
                                                                                psi <- HP.getPageIndex
                                                                                pst <- HP.getPageText
                                                                                HP.setPageText ("old "++ show x) 0
                                                                                return (x, psi, pst)
                                            -- liftDebugIO pss
                                            return . ((pc == i+1) &&) $ all (\(k, ki, kt) ->
                                                                                ki == (i-k) &&
                                                                                kt == show k) $ pss
prop_set_page_index_fail hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            replicateM (i-1) HP.addPage
                                            shouldFail $ HP.setPageIndex i

prop_save_page, prop_save_page_fail, prop_save_page_as :: HPS.ServerHandle -> String -> Property
prop_save_page hps file =
    file /= "" ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            let path = testDir ++ "/Test" ++ file
                                            Hint.liftIO $ writeFile path file
                                            HP.closeAllPages
                                            HP.openPage path
                                            HP.savePage
                                            p1 <- HP.getPageText
                                            HP.openPage path
                                            p2 <- HP.getPageText
                                            HP.addExpr file
                                            HP.savePage
                                            p3 <- HP.getPageText
                                            return $ p1 == file &&
                                                     p2 == file &&
                                                     p3 == (file ++ "\n\n" ++ file)
prop_save_page_fail hps file =
    file /= "" ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            shouldFail $ HP.savePage
prop_save_page_as hps file =
    file /= "" ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            let path = testDir ++ "/Test" ++ file
                                            HP.closeAllPages
                                            HP.setPageText file 0
                                            HP.savePageAs path
                                            HP.openPage path
                                            p0 <- HP.getPageText
                                            HP.savePage
                                            HP.openPage path
                                            p1 <- HP.getPageText
                                            return $ p0 == file &&
                                                     p1 == file

prop_close_page :: HPS.ServerHandle -> Int -> Property
prop_close_page hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            HP.setPageText (show i) 0
                                            forM [1..i] $ \x ->
                                                            do
                                                                HP.addPage
                                                                HP.setPageText (show (i-x)) 0 
                                            pcb <- HP.getPageCount
                                            pss <- (flip mapM) [i,i-1..1] $ \x -> do
                                                                                    HP.setPageIndex x
                                                                                    pbi <- HP.getPageIndex
                                                                                    pbt <- HP.getPageText
                                                                                    HP.closePage
                                                                                    pai <- HP.getPageIndex
                                                                                    pat <- HP.getPageText
                                                                                    return (x, pbi, pbt, pai, pat)
                                            pca <- HP.getPageCount
                                            pia <- HP.getPageIndex
                                            pta <- HP.getPageText
                                            HP.closePage
                                            pcf <- HP.getPageCount
                                            pif <- HP.getPageIndex
                                            ptf <- HP.getPageText
                                            --liftDebugIO (pcb, (pca, pia, pta), (pcf, pif, ptf), pss)
                                            --let ff = \(k, _, _, _, _) -> (k, k, show k, k-1, show (k-1))
                                            --liftDebugIO (i+1, (1, 0, "0"), (1, 0, ""), map ff pss)
                                            return . ((pcb == i+1 &&
                                                       pca == 1 && pia == 0 && pta == "0" &&
                                                       pcf == 1 && pif == 0 && ptf == "") &&) $
                                                all (\(k, kbi, kbt, kai, kat) ->
                                                        kbi == k &&
                                                        kbt == show k &&
                                                        kai == k-1 &&
                                                        kat == show (k-1) ) $ pss

prop_is_modified_page :: HPS.ServerHandle -> FilePath -> Property
prop_is_modified_page hps file =
    file /= "" ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            let path = testDir ++ "/Test" ++ file
                                            HP.addPage
                                            f0 <- HP.isModifiedPage
                                            HP.setPageText file 0
                                            HP.addExpr file
                                            t0 <- HP.isModifiedPage
                                            HP.undo
                                            t1 <- HP.isModifiedPage
                                            HP.undo
                                            f1 <- HP.isModifiedPage
                                            HP.redo
                                            t2 <- HP.isModifiedPage
                                            HP.redo
                                            t3 <- HP.isModifiedPage
                                            HP.savePageAs path
                                            f2 <- HP.isModifiedPage
                                            HP.undo
                                            t4 <- HP.isModifiedPage
                                            HP.addExpr $ file ++ "$$$"
                                            t5 <- HP.isModifiedPage
                                            HP.openPage path
                                            f3 <- HP.isModifiedPage
                                            HP.redo
                                            f4 <- HP.isModifiedPage
                                            let result = not (f0 || f1 || f2 || f3 || f4) &&
                                                         t0 && t1 && t2 && t3 && t4 && t5
                                            if not result
                                                then do
                                                        liftDebugIO $ (('F', f0, f1, f2, f3, f4),
                                                                       ('T', t0, t1, t2, t3, t4, t5))
                                                        return False
                                                else
                                                    return True 

prop_save_nth_page, prop_save_nth_page_fail :: HPS.ServerHandle -> Int -> Property
prop_save_nth_page hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            forM [1..i] $ \x ->
                                                            do
                                                                let y = show $ i - x
                                                                    path = testDir ++ "/Test" ++ y
                                                                Hint.liftIO $ writeFile path y
                                                                HP.openPage path
                                            forM [0..i-1] $ \x ->
                                                                do
                                                                    HP.setPageText (show x) 0
                                                                    HP.savePageNth x
                                            HP.closeAllPages
                                            (flip allM) [0..i-1] $ \x ->
                                                                    do
                                                                        let path = testDir ++ "/Test" ++ show x
                                                                        HP.openPage path
                                                                        liftM ((show x) ==) $ HP.getPageText
prop_save_nth_page_fail hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            a <- shouldFail $ HP.savePageNth 0
                                            forM [1..i] $ \_ -> HP.addPage
                                            b <- (flip allM) [1..i] $ shouldFail . HP.savePageNth
                                            c <- shouldFail $ HP.savePageNth $ i + 1
                                            return $ a && b && c

prop_save_nth_page_as, prop_save_nth_page_as_fail :: HPS.ServerHandle -> Int -> Property
prop_save_nth_page_as hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            forM [1..i] $ \x ->
                                                            do
                                                                let y = show $ i - x
                                                                HP.addPage
                                                                HP.setPageText y 0
                                            forM [0..i-1] $ \x ->
                                                                do
                                                                    let path = testDir ++ "/Test" ++ show x
                                                                    HP.savePageNthAs x path
                                            HP.closeAllPages
                                            (flip allM) [0..i-1] $ \x ->
                                                                    do
                                                                        let path = testDir ++ "/Test" ++ show x
                                                                        HP.openPage path
                                                                        liftM ((show x) ==) $ HP.getPageText
prop_save_nth_page_as_fail hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            forM [1..i] $ \_ -> HP.addPage
                                            a <- shouldFail $ HP.savePageNth $ i + 1
                                            b <- shouldFail $ HP.savePageNth $ -1
                                            return $ a && b

prop_is_modified_nth_page, prop_is_modified_nth_page_fail :: HPS.ServerHandle -> Int -> Property
prop_is_modified_nth_page hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            forM [1..i] $ \x ->
                                                            do
                                                                let y = show $ i - x
                                                                HP.addPage
                                                                HP.setPageText y 0
                                            t0 <- allM HP.isModifiedPageNth [0..i-1]
                                            f0 <- HP.isModifiedPageNth i 
                                            let path = testDir ++ "/Test.page"
                                            forM [0..i-1] $ flip HP.savePageNthAs $ path
                                            f1 <- allM HP.isModifiedPageNth [0..i-1]
                                            return $ t0 && not (f0 || f1)
prop_is_modified_nth_page_fail hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            forM [1..i] $ \_ -> HP.addPage
                                            a <- shouldFail $ HP.isModifiedPageNth $ i + 1
                                            b <- shouldFail $ HP.isModifiedPageNth $ -1
                                            return $ a && b

prop_close_nth_page, prop_close_nth_page_fail :: HPS.ServerHandle -> Int -> Property
prop_close_nth_page  hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            forM [1..i] $ \_ -> HP.addPage
                                            t0 <- (flip allM) [1..i] $ \x ->
                                                                        do
                                                                            let y = i - x
                                                                            HP.closePageNth y
                                                                            pgc <- liftM (y+1==) $ HP.getPageCount
                                                                            pgi <- liftM (0 ==) $ HP.getPageIndex
                                                                            let result = pgc && pgi
                                                                            if not result
                                                                                then
                                                                                    do
                                                                                        liftDebugIO [pgc, pgi]
                                                                                        return False
                                                                                else
                                                                                    return True
                                            HP.closeAllPages
                                            HP.setPageText "two" 0
                                            HP.addPage
                                            HP.setPageText "one" 0
                                            HP.addPage
                                            HP.setPageText "zero" 0
                                            HP.setPageIndex 1
                                            HP.closePageNth 1
                                            t1 <- liftM ("zero" ==) $ HP.getPageText
                                            t2 <- liftM (0 ==) $ HP.getPageIndex
                                            HP.setPageIndex 1
                                            t3 <- liftM ("two" ==) $ HP.getPageText
                                            HP.closePageNth 1
                                            HP.closePageNth 0
                                            t4 <- liftM ("" ==) $ HP.getPageText
                                            t5 <- liftM (0 ==) $ HP.getPageIndex
                                            t6 <- liftM (1 ==) $ HP.getPageCount
                                            let result = t0 && t1 && t2 && t3 && t4 && t5 && t6
                                            if not result
                                                then
                                                    do
                                                        liftDebugIO [t0, t1, t2, t3, t4, t5, t6]
                                                        return False
                                                else
                                                    return True
prop_close_nth_page_fail hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            forM [1..i] $ \_ -> HP.addPage
                                            a <- shouldFail $ HP.closePageNth $ i + 1
                                            b <- shouldFail $ HP.closePageNth $ -1
                                            return $ a && b

prop_close_all_pages :: HPS.ServerHandle -> Int -> Property
prop_close_all_pages hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            c0 <- HP.getPageCount
                                            HP.setPageText "not empty" 0
                                            forM [1..i-1] $ \_ -> HP.addPage
                                            c1 <- HP.getPageCount
                                            HP.setPageIndex $ c1 - 1
                                            HP.closeAllPages
                                            c2 <- HP.getPageCount
                                            i2 <- HP.getPageIndex
                                            t2 <- HP.getPageText
                                            let result = (c0, c1, c2, i2, t2) == (1, i, 1, 0, "") 
                                            if not result
                                                then
                                                    do
                                                        liftDebugIO (c0, c1, c2, i2, t2)
                                                        return False
                                                else
                                                    return True

prop_safe_save_page_as :: HPS.ServerHandle -> ModuleName -> Bool
prop_safe_save_page_as hps (MN file) =
    unsafePerformIO $ HPS.runIn hps $ do
                                        let path = testDir ++ "/" ++ file
                                        Hint.liftIO $ removeFileMayNotExist path
                                        HP.closeAllPages
                                        HP.setPageText file 0
                                        HP.safeSavePageAs path
                                        HP.openPage path
                                        p0 <- HP.getPageText
                                        p1 <- shouldFail $ HP.safeSavePageAs path
                                        HP.openPage path
                                        p2 <- HP.getPageText
                                        return $ p0 == file && p1 && p2 == file
    where removeFileMayNotExist f = do
                                        e <- doesFileExist f
                                        if e then removeFile f else return ()

prop_safe_close_page :: HPS.ServerHandle -> Int -> Property
prop_safe_close_page hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ (flip allM) [1..i] $ \_ ->
                                                                do
                                                                    HP.addPage -- To avoid page 0
                                                                    HP.addPage
                                                                    HP.setPageText (show i) 0
                                                                    c0 <- HP.getPageCount
                                                                    t1 <- shouldFail $ HP.safeClosePage
                                                                    c1 <- HP.getPageCount
                                                                    HP.undo
                                                                    HP.safeClosePage
                                                                    c2 <- HP.getPageCount
                                                                    return $ c0 == c1 &&
                                                                             c0 == (c2 + 1) &&
                                                                             t1

prop_safe_save_nth_page_as :: HPS.ServerHandle -> Int -> Property
prop_safe_save_nth_page_as hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            forM [1..i] $ \x ->
                                                            do
                                                                let y = show $ i - x
                                                                HP.addPage
                                                                HP.setPageText y 0
                                            let path = testDir ++ "/Test"
                                            Hint.liftIO $ Str.writeFile path $ Str.pack $ show i
                                            t0 <- (flip allM) [0..i-1] $ shouldFail . ((flip HP.safeSavePageNthAs) path)
                                            Hint.liftIO $ removeFile path
                                            HP.safeSavePageNthAs 0 path
                                            return t0

prop_safe_close_nth_page :: HPS.ServerHandle -> Int -> Property
prop_safe_close_nth_page hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            forM [1..i] $ \x ->
                                                            do
                                                                let y = show $ i - x
                                                                HP.addPage
                                                                HP.setPageText y 0
                                            t0 <- allM (shouldFail . HP.safeClosePageNth) [0..i-2]
                                            HP.addPage
                                            HP.safeClosePageNth 0
                                            t1 <- allM HP.isModifiedPageNth [1..i-1]
                                            return $ t0 && t1

prop_safe_close_all_pages :: HPS.ServerHandle -> Int -> Property
prop_safe_close_all_pages hps i =
    i > 0 ==>
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            forM [1..i] $ \x ->
                                                            do
                                                                let y = show $ i - x
                                                                HP.addPage
                                                                HP.setPageText y 0
                                            shouldFail HP.safeCloseAllPages

prop_let_valueOf :: HPS.ServerHandle -> HS.ServerHandle -> String -> Bool
prop_let_valueOf hps hs txt =
    unsafePerformIO $ do
                        let expr = "length \"" ++ txt ++ "\""
                        (hpsr1, hpsr2) <- HPS.runIn hps $ do
                                                            HP.addPage
                                                            HP.addExpr $ "testL = " ++ expr
                                                            HP.addExpr $ "test2L x = 2 * x"
                                                            HP.addExpr "test2L testL"
                                                            r1 <- HP.valueOf
                                                            HP.addExpr $ "(test2L testL) `div` 2"
                                                            r2 <- HP.valueOf
                                                            return (r1, r2)
                        hsr1 <- HS.runIn hs $ Hint.eval $ "2 * " ++ expr
                        hsr2 <- HS.runIn hs $ Hint.eval $ expr
                        -- liftDebugIO [(hpsr1, hpsr2), (hsr1, hsr2)]
                        return $ (hpsr1, hpsr2) == (hsr1, hsr2)

prop_let_typeOf :: HPS.ServerHandle -> HS.ServerHandle -> String -> Property
prop_let_typeOf hps hs txt = txt /= "" ==>
    unsafePerformIO $ do
                        let expr = "\"" ++ txt ++ "\""
                        (hpsr1, hpsr2) <- HPS.runIn hps $ do
                                                            HP.addPage
                                                            HP.addExpr $ "testL = " ++ expr
                                                            HP.addExpr "testL"
                                                            r1 <- HP.typeOf
                                                            HP.addExpr $ "test2L x = length x"
                                                            HP.addExpr $ "2 * (test2L testL)"
                                                            r2 <- HP.typeOf
                                                            return (r1, r2)
                        hsr1 <- HS.runIn hs $ Hint.typeOf expr
                        hsr2 <- HS.runIn hs $ Hint.typeOf $ "2 * (length " ++ expr ++ ")"
                        -- liftDebugIO [(hpsr1, hpsr2), (hsr1, hsr2)]
                        return $ (hpsr1, hpsr2) == (hsr1, hsr2)

prop_let_fail :: HPS.ServerHandle -> HS.ServerHandle -> String -> Bool
prop_let_fail hps hs txt =
    unsafePerformIO $ do
                        let expr = "testL x = lenggth x"
                        Left hpsr <- HPS.runIn hps $ do
                                                        HP.addPage
                                                        HP.addExpr expr
                                                        HP.addExpr $ "test2L = 2 * (testL \"" ++ txt ++ "\")"
                                                        HP.addExpr $ "test2L / 2"
                                                        HP.valueOf
                        Left hsr <- HS.runIn hs $ Hint.eval "lenggth \"\""
                        return $ hsr == hpsr
    
prop_get_available_extensions :: String -> Bool
prop_get_available_extensions _ = HP.availableExtensions == Hint.availableExtensions

prop_get_set_extensions :: HPS.ServerHandle -> [HP.Extension] -> Bool
prop_get_set_extensions hps exs =
    unsafePerformIO $ HPS.runIn hps $ do
                                        HP.setLanguageExtensions []
                                        exs0 <- HP.getLanguageExtensions
                                        HP.setLanguageExtensions exs
                                        exs1 <- HP.getLanguageExtensions
                                        HP.setLanguageExtensions []
                                        exs2 <- HP.getLanguageExtensions
                                        return $ (exs0 == Right []) && (exs1 == Right exs) && (exs2 == Right [])

prop_working_extensions :: HPS.ServerHandle -> WorkingExtension -> Bool
prop_working_extensions hps (WEX es m) =
    unsafePerformIO $ HPS.runIn hps $ do
                                        path <- Hint.liftIO . getDataFileName $ "res/test/" ++ m
                                        HP.setLanguageExtensions []
                                        before <- HP.loadModules [path]
                                        HP.setLanguageExtensions es
                                        after <- HP.loadModules [path]
                                        let failed = case before of
                                                        Left _ -> True
                                                        _ -> False
                                        -- liftDebugIO (before, after, failed)
                                        return $ failed && (after == Right ())

prop_get_set_extension_fail :: HPS.ServerHandle -> String -> Bool
prop_get_set_extension_fail hps s =
    unsafePerformIO $ HPS.runIn hps $ do
                                        r <- HP.setLanguageExtensions [HP.UnknownExtension s]
                                        case r of
                                            Left _ -> return True
                                            Right _ -> return False

prop_get_set_source_dirs :: HPS.ServerHandle -> [FilePath] -> Bool
prop_get_set_source_dirs hps sds =
    unsafePerformIO $ HPS.runIn hps $ do
                                        HP.setSourceDirs []
                                        sds0 <- HP.getSourceDirs
                                        HP.setSourceDirs sds
                                        sds1 <- HP.getSourceDirs
                                        HP.setSourceDirs []
                                        sds2 <- HP.getSourceDirs
                                        return $ (sds0 == []) && (sds1 == sds) && (sds2 == [])

prop_working_source_dirs :: HPS.ServerHandle -> ModuleName -> Bool
prop_working_source_dirs hps (MN file) =
    unsafePerformIO $ HPS.runIn hps $ do
                                        let path = testDir ++ "/" ++ file ++ ".hs"
                                        HP.setPageText ("module " ++ file ++ " where t = 1") 0
                                        HP.savePageAs path
                                        HP.setSourceDirs []
                                        before <- HP.loadModules [file]
                                        HP.setSourceDirs [testDir, testDir]
                                        after <- HP.loadModules [file]
                                        let failed = case before of
                                                        Left _ -> True
                                                        _ -> False
                                        -- liftDebugIO (before, after, failed)
                                        return $ failed && (after == Right ())


prop_get_set_ghc_opts :: HPS.ServerHandle -> String -> Bool
prop_get_set_ghc_opts hps ops =
    unsafePerformIO $ HPS.runIn hps $ do
                                        ops0 <- HP.getGhcOpts
                                        HP.setGhcOpts $ "-i" ++ ops
                                        ops1 <- HP.getGhcOpts
                                        HP.setGhcOpts ""
                                        ops2 <- HP.getGhcOpts
                                        -- liftDebugIO (ops, ops0, ops1, ops2)
                                        return $ (ops1 == (ops0 ++ " -i" ++ ops)) && (ops2 == ops1)

prop_get_set_ghc_opts_fail :: HPS.ServerHandle -> ClassName -> Bool
prop_get_set_ghc_opts_fail hps (CN ops) =
    unsafePerformIO $ HPS.runIn hps $ do
                                        res <- HP.setGhcOpts ops
                                        case res of
                                            Left _ -> return True
                                            _ -> return False

prop_working_ghc_opts :: HPS.ServerHandle -> ModuleName -> Bool
prop_working_ghc_opts hps (MN file) =
    unsafePerformIO $ HPS.runIn hps $ do
                                        let path = testDir ++ "/" ++ file ++ ".hs"
                                        HP.setPageText ("module " ++ file ++ " where t = 1") 0
                                        HP.savePageAs path
                                        HP.setSourceDirs []
                                        before <- HP.loadModules [file]
                                        HP.setGhcOpts $ "-i" ++ testDir
                                        after <- HP.loadModules [file]
                                        let failed = case before of
                                                        Left _ -> True
                                                        _ -> False
                                        -- liftDebugIO (before, after, failed)
                                        return $ failed && (after == Right ())

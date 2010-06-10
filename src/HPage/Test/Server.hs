module HPage.Test.Server where

import Data.Char
import GHC.IO
import Control.Monad.Error
import Test.QuickCheck
import Test.Runner.Driver
import Test.Runner
import qualified HPage.Control as HP
import qualified HPage.Server as HPS
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Server as HS
import System.Directory
import System.FilePath
-- import HPage.Utils.Log
import Paths_hpage

newtype ModuleName = MN {mnString :: String}
    deriving (Eq)

instance Show ModuleName where
    show = mnString

instance Arbitrary ModuleName where
    arbitrary = do
                    s <- arbitrary
                    return . MN $ "Test" ++ map toLower s

newtype KnownModuleName = KMN {kmnString :: String}
    deriving (Eq, Show)

instance Arbitrary KnownModuleName where
    arbitrary = elements $ map KMN ["Data.List", "Control.Monad", "System.Directory", "Control.Monad.Loops"]
    

newtype ClassName = CN {cnString :: String}
    deriving (Eq, Show)

instance Arbitrary ClassName where
    arbitrary = elements $ map CN ["HPage", "IO", "IO a", "Int", "String"]

instance Arbitrary HP.Extension where
    arbitrary = elements HP.availableExtensions

data WorkingExtension = WEX {wexExts :: [HP.Extension],
                             wexModule :: String}
    deriving (Eq, Show)
    
instance Arbitrary WorkingExtension where
    arbitrary = elements [wexTypeSynonymInstances, wexOverlappingInstances, wexFlexibleInstances]

wexTypeSynonymInstances :: WorkingExtension
wexTypeSynonymInstances = WEX [HP.TypeSynonymInstances] "TypeSynonymInstances.hs"

wexOverlappingInstances :: WorkingExtension
wexOverlappingInstances = WEX [HP.TypeSynonymInstances,
                               HP.OverlappingInstances] "OverlappingInstances.hs"

wexFlexibleInstances :: WorkingExtension
wexFlexibleInstances = WEX [HP.FlexibleInstances] "FlexibleInstances.hs"

shouldFail :: (MonadError e m) => m a -> m Bool
shouldFail a = (a >> return False) `catchError` (\_ -> return True)

testDir :: FilePath
testDir = "TestFiles"

main :: IO ()
main =
    do
        createDirectoryIfMissing True testDir
        hps <- HPS.start
        hs <- HS.start
        _ <- runTests [("Editing", runWithQuickCheck $ prop_setget_text hps)
                     , ("Many Pages 1", runWithQuickCheck $ prop_new_page hps)
                     , ("Many Pages 2", runWithQuickCheck $ prop_open_page hps)
                     , ("Many Pages 3", runWithQuickCheck $ prop_open_page_fail hps)
                     , ("Many Pages 4", runWithQuickCheck $ prop_setget_page hps)
                     , ("Many Pages 5", runWithQuickCheck $ prop_set_page_index_fail hps)
                     , ("Many Pages 6", runWithQuickCheck $ prop_save_page hps)
                     , ("Many Pages 7", runWithQuickCheck $ prop_save_page_fail hps)
                     , ("Many Pages 8", runWithQuickCheck $ prop_save_page_as hps)
                     , ("Many Pages 9", runWithQuickCheck $ prop_close_page hps)
                     , ("Named Expressions", runWithQuickCheck $ prop_let_fail hps hs)
                     , ("Expressions 1", runWithQuickCheck $ prop_fail hps hs)
                     , ("Expressions 2", runWithQuickCheck $ prop_load_module hps hs)
                     , ("Expressions 3", runWithQuickCheck $ prop_import_module hps)
                     , ("Expressions 4", runWithQuickCheck $ prop_reload_modules hps hs)
                     , ("Expressions 5", runWithQuickCheck $ prop_get_loaded_modules hps hs)
                     , ("Expressions 6", runWithQuickCheck $ prop_get_module_exports hps hs)
                     , ("Extensions 1", runWithQuickCheck prop_get_available_extensions)
                     , ("Extensions 2", runWithQuickCheck $ prop_get_set_extensions hps)
                     , ("Extensions 3", runWithQuickCheck $ prop_working_extensions hps)
                     , ("Extensions 4", runWithQuickCheck $ prop_get_set_extension_fail hps)
                     , ("Src. Dirs. 1", runWithQuickCheck $ prop_get_set_source_dirs hps)
                     , ("Src. Dirs. 2", runWithQuickCheck $ prop_working_source_dirs hps)
                     , ("GHC Options 1", runWithQuickCheck $ prop_get_set_ghc_opts hps)
                     , ("GHC Options 2", runWithQuickCheck $ prop_get_set_ghc_opts_fail hps)
                     , ("GHC Options 3", runWithQuickCheck $ prop_working_ghc_opts hps)]
                 
        removeDirectoryRecursive testDir
                    
instance Eq (Hint.InterpreterError) where
    a == b = show a == show b

prop_fail :: HPS.ServerHandle -> HS.ServerHandle -> String -> Bool
prop_fail hps hs txt =
    unsafePerformIO $ do
                        let expr = "lenggth \"" ++ txt ++ "\""
                        Left hpsr <- HPS.runIn hps $ HP.setPageText expr 0 >> HP.interpret
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
          match _ = False

prop_load_module :: HPS.ServerHandle -> HS.ServerHandle -> ModuleName -> Bool
prop_load_module hps hs mn =
    (show mn == "Test") || (
        unsafePerformIO $ do
                            let mname = testDir ++ "." ++ show mn
                            let ftxt = "module " ++ mname ++ " where v = 32"
                            let f2txt = "v = \"" ++ show mn ++ "\""
                            hpsr <- HPS.runIn hps $ do
                                                        -- Save TestFiles/Test...hs
                                                        _ <- HP.setPageText ftxt 0
                                                        HP.savePageAs $ testDir </> (show mn) ++ ".hs"
                                                        -- Save TestFiles/test.hs
                                                        _ <- HP.setPageText f2txt 0
                                                        HP.savePageAs $ testDir </> "test.hs"
                                                        -- Load TestFiles/test.hs by path
                                                        _ <- HP.loadModules [testDir </> "test.hs"]
                                                        _ <- HP.setPageText "v" 0
                                                        Right fv <- HP.interpret
                                                        fm <- HP.getLoadedModules >>= return . mN
                                                        -- Load TestFiles/Test...hs by name
                                                        _ <- HP.loadModules [mname]
                                                        _ <- HP.setPageText "v" 0
                                                        Right sv <- HP.interpret
                                                        sm <- HP.getLoadedModules >>= return . mN
                                                        return (HP.intValue fv, HP.intValue sv, fm, sm)
                            hsr <- HS.runIn hs $ do
                                                    Hint.loadModules [testDir </> "test.hs"]
                                                    Hint.getLoadedModules >>= Hint.setTopLevelModules
                                                    fv <- Hint.eval "v"
                                                    fm <- Hint.getLoadedModules
                                                    Hint.loadModules [mname]
                                                    Hint.getLoadedModules >>= Hint.setTopLevelModules
                                                    sv <- Hint.eval "v"
                                                    sm <- Hint.getLoadedModules
                                                    return (fv, sv, Right fm, Right sm)
                            -- liftDebugIO (hpsr, hsr)
                            return $ Right hpsr == hsr )
        where mN (Right mns) = Right $ map HP.modName mns
              mN (Left err) = Left err
                            

prop_reload_modules :: HPS.ServerHandle -> HS.ServerHandle -> String -> Bool
prop_reload_modules hps hs txt =
    unsafePerformIO $ do
                        let expr = "test = show \"" ++ txt ++ "\"" 
                        Right hpsr <- HPS.runIn hps $ do
                                                    _ <- HP.setPageText expr 0
                                                    HP.savePageAs $ testDir </> "test.hs"
                                                    _ <- HP.setPageText "test" 0
                                                    _ <- HP.loadModules [testDir </> "test.hs"]
                                                    _ <- HP.reloadModules
                                                    HP.interpret
                        Right hsr <- HS.runIn hs $ do
                                                Hint.loadModules [testDir </> "test.hs"]
                                                Hint.getLoadedModules >>= Hint.setTopLevelModules
                                                Hint.eval "test"
                        return $ HP.intValue hpsr == hsr
    
prop_get_loaded_modules :: HPS.ServerHandle -> HS.ServerHandle -> ModuleName -> Bool
prop_get_loaded_modules hps hs mn =
    unsafePerformIO $ do
                        let expr1 = "ytest = show \"" ++ show mn ++ "\""
                        let expr2 = "module " ++ testDir ++ ".XX" ++ show mn ++ "2 where import " ++ testDir ++ ".XX" ++ show mn ++ "3; xtest = show \"" ++ show mn ++ "\""
                        let expr3 = "module " ++ testDir ++ ".XX" ++ show mn ++ "3 where xfact = (1,2,3)"
                        let mnf1  = testDir </> "XX" ++ (show mn) ++ "1.hs"
                        let mnf2  = testDir </> "XX" ++ (show mn) ++ "2.hs"
                        let mnf3  = testDir </> "XX" ++ (show mn) ++ "3.hs"
                        HPS.runIn hps $ do
                                            _ <- HP.setPageText expr1 0
                                            HP.savePageAs mnf1
                                            _ <- HP.setPageText expr2 0
                                            HP.savePageAs mnf2
                                            _ <- HP.setPageText expr3 0
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

prop_setget_text :: HPS.ServerHandle -> String -> Bool
prop_setget_text hps txt =
    unsafePerformIO $ HPS.runIn hps $ do
                                        _ <- HP.setPageText txt 0
                                        HP.getPageText >>= return . (txt ==)

prop_undoredo :: HPS.ServerHandle -> String -> Bool
prop_undoredo hps txt =
    txt == "" || (
    unsafePerformIO $ HPS.runIn hps $ do 
                                        HP.addPage
                                        b0 <- HP.getPageText
                                        _ <- HP.setPageText b0 0
                                        b1 <- HP.getPageText
                                        addExpr "xx"
                                        b2 <- HP.getPageText
                                        addExpr "yy"
                                        b3 <- HP.getPageText
                                        _ <- HP.setPageText b3 6
                                        b4 <- HP.getPageText
                                        addExpr "zz"
                                        b5 <- HP.getPageText
                                        let to5 = [0..5] :: [Int]
                                        after <- mapM (\_ -> HP.undo >> HP.getPageText) to5
                                        redo <- mapM (\_ -> HP.redo >> HP.getPageText) to5
                                        _ <- HP.setPageText "" 0 >> HP.setPageText "cc" 0 >> HP.setPageText "" 0
                                        c0 <- addExpr "zz" >> HP.getPageText
                                        c1 <- HP.undo >> HP.getPageText
                                        c2 <- HP.undo >> HP.getPageText
                                        c3 <- HP.setPageText "ww" 0 >> HP.getPageText
                                        c4 <- HP.redo >> HP.getPageText
                                        let result = ([b4, b3, b2, b1, b0, "", ""] == after) &&
                                                     ([b1, b2, b3, b4, b5, b5, b5] == redo) &&
                                                     ([c0, c1, c2, c3, c4] == ["zz", "", "cc", "ww", "ww"])
                                        if not result
                                            then
                                                do
                                                    -- liftDebugIO [b5, b4, b3, b2, b1, b0, "", ""]
                                                    -- liftDebugIO after
                                                    -- liftDebugIO [b1, b2, b3, b4, b5, b5, b5, b5]
                                                    -- liftDebugIO redo
                                                    -- liftDebugIO ["zz", "", "cc", "ww", "ww"]
                                                    -- liftDebugIO [c0, c1, c2, c3, c4]
                                                    return False
                                            else
                                                return True )

prop_new_page :: HPS.ServerHandle -> Int -> Bool
prop_new_page hps i =
    (not (i > 0)) || (
        unsafePerformIO $ HPS.runIn hps $ do
                                            _ <- HP.setPageText "" 0
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
                                                                                _ <- HP.setPageText ("old "++ show x) 0
                                                                                return (x, psc, psi, pst)
                                            let results = (0,pc0,pi0,pt0):pss
                                            -- liftDebugIO results
                                            return $ all (\(k, kc, ki, kt) ->
                                                            kc == k+1 &&
                                                            ki == 0 &&
                                                            kt == "") $ results )

prop_open_page, prop_open_page_fail :: HPS.ServerHandle -> String -> Bool
prop_open_page hps file =
    file == "" || (
        unsafePerformIO $ HPS.runIn hps $ do
                                            let path = testDir </> "Test" ++ file
                                            Hint.liftIO $ writeFile path file
                                            HP.closeAllPages
                                            HP.openPage path
                                            liftM (file ==) HP.getPageText )

prop_open_page_fail hps file =
    file == "" || (
        unsafePerformIO $ HPS.runIn hps $ do
                                            let path = testDir </> "NO-Test" ++ file
                                            HP.closeAllPages
                                            shouldFail $ HP.openPage path )

prop_setget_page, prop_set_page_index_fail :: HPS.ServerHandle -> Int -> Bool
prop_setget_page hps i =
    (not (i > 0)) || (
        unsafePerformIO $ HPS.runIn hps $ do
                                            _ <- HP.setPageText "" 0
                                            HP.closeAllPages
                                            _ <- HP.setPageText "0" 0
                                            forM_ [1..i] $ \x ->
                                                            do
                                                                HP.addPage
                                                                HP.setPageText (show x) 0 
                                            pc <- HP.getPageCount
                                            pss <- (flip mapM) [0..i] $ \x -> do
                                                                                HP.setPageIndex (i-x)
                                                                                psi <- HP.getPageIndex
                                                                                pst <- HP.getPageText
                                                                                _ <- HP.setPageText ("old "++ show x) 0
                                                                                return (x, psi, pst)
                                            -- liftDebugIO pss
                                            return . ((pc == i+1) &&) $ all (\(k, ki, kt) ->
                                                                                ki == (i-k) &&
                                                                                kt == show k) $ pss )
prop_set_page_index_fail hps i =
    (not (i > 0)) || (
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            replicateM_ (i-1) HP.addPage
                                            shouldFail $ HP.setPageIndex i )

prop_save_page, prop_save_page_fail, prop_save_page_as :: HPS.ServerHandle -> String -> Bool
prop_save_page hps file =
    file == "" || (
        unsafePerformIO $ HPS.runIn hps $ do
                                            let path = testDir </> "Test" ++ file
                                            Hint.liftIO $ writeFile path file
                                            HP.closeAllPages
                                            HP.openPage path
                                            HP.savePage
                                            p1 <- HP.getPageText
                                            HP.openPage path
                                            p2 <- HP.getPageText
                                            addExpr file
                                            HP.savePage
                                            p3 <- HP.getPageText
                                            return $ p1 == file &&
                                                     p2 == file &&
                                                     p3 == (file ++ "\n\n" ++ file) )
prop_save_page_fail hps file =
    file == "" || (
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            shouldFail $ HP.savePage )
prop_save_page_as hps file =
    file == "" || (
        unsafePerformIO $ HPS.runIn hps $ do
                                            let path = testDir </> "Test" ++ file
                                            HP.closeAllPages
                                            _ <- HP.setPageText file 0
                                            HP.savePageAs path
                                            HP.openPage path
                                            p0 <- HP.getPageText
                                            HP.savePage
                                            HP.openPage path
                                            p1 <- HP.getPageText
                                            return $ p0 == file &&
                                                     p1 == file )

prop_close_page :: HPS.ServerHandle -> Int -> Bool
prop_close_page hps i =
    (not (i > 0)) || (
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            _ <- HP.setPageText (show i) 0
                                            forM_ [1..i] $ \x ->
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
                                                        kat == show (k-1) ) $ pss )

prop_close_all_pages :: HPS.ServerHandle -> Int -> Bool
prop_close_all_pages hps i =
    (not (i > 0)) || (
        unsafePerformIO $ HPS.runIn hps $ do
                                            HP.closeAllPages
                                            c0 <- HP.getPageCount
                                            _ <- HP.setPageText "not empty" 0
                                            forM_ [1..i-1] $ \_ -> HP.addPage
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
                                                        -- liftDebugIO (c0, c1, c2, i2, t2)
                                                        return False
                                                else
                                                    return True )

prop_let_fail :: HPS.ServerHandle -> HS.ServerHandle -> String -> Bool
prop_let_fail hps hs txt =
    unsafePerformIO $ do
                        let expr = "testL x = lenggth x"
                        Left hpsr <- HPS.runIn hps $ do
                                                        HP.addPage
                                                        addExpr expr
                                                        addExpr $ "test2L = 2 * (testL \"" ++ txt ++ "\")"
                                                        addExpr "test2L / 2"
                                                        HP.interpret
                        Left hsr <- HS.runIn hs $ Hint.eval "lenggth \"\""
                        return $ hsr == hpsr
    
prop_get_available_extensions :: String -> Bool
prop_get_available_extensions _ = HP.availableExtensions == Hint.availableExtensions

prop_get_set_extensions :: HPS.ServerHandle -> [HP.Extension] -> Bool
prop_get_set_extensions hps exs =
    unsafePerformIO $ HPS.runIn hps $ do
                                        _ <- HP.setLanguageExtensions []
                                        exs0 <- HP.getLanguageExtensions
                                        _ <- HP.setLanguageExtensions exs
                                        exs1 <- HP.getLanguageExtensions
                                        _ <- HP.setLanguageExtensions []
                                        exs2 <- HP.getLanguageExtensions
                                        return $ (exs0 == Right []) && (exs1 == Right exs) && (exs2 == Right [])

prop_working_extensions :: HPS.ServerHandle -> WorkingExtension -> Bool
prop_working_extensions hps (WEX es m) =
    unsafePerformIO $ HPS.runIn hps $ do
                                        path <- Hint.liftIO . getDataFileName $ "res" </> "test" </> m
                                        _ <- HP.setLanguageExtensions []
                                        before <- HP.loadModules [path]
                                        _ <- HP.setLanguageExtensions es
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
                                        _ <- HP.setSourceDirs []
                                        sds0 <- HP.getSourceDirs
                                        _ <- HP.setSourceDirs sds
                                        sds1 <- HP.getSourceDirs
                                        _ <- HP.setSourceDirs []
                                        sds2 <- HP.getSourceDirs
                                        return $ (sds0 == []) && (sds1 == sds) && (sds2 == [])

prop_working_source_dirs :: HPS.ServerHandle -> ModuleName -> Bool
prop_working_source_dirs hps (MN file) =
    unsafePerformIO $ HPS.runIn hps $ do
                                        let path = testDir </> file ++ ".hs"
                                        _ <- HP.setPageText ("module " ++ file ++ " where t = 1") 0
                                        HP.savePageAs path
                                        _ <- HP.setSourceDirs []
                                        before <- HP.loadModules [file]
                                        _ <- HP.setSourceDirs [testDir, testDir]
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
                                        _ <- HP.setGhcOpts $ "-i" ++ ops
                                        ops1 <- HP.getGhcOpts
                                        _ <- HP.setGhcOpts ""
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
                                        let path = testDir </> file ++ ".hs"
                                        _ <- HP.setPageText ("module " ++ file ++ " where t = 1") 0
                                        HP.savePageAs path
                                        _ <- HP.setSourceDirs []
                                        before <- HP.loadModules [file]
                                        _ <- HP.setGhcOpts $ "-i" ++ testDir
                                        after <- HP.loadModules [file]
                                        let failed = case before of
                                                        Left _ -> True
                                                        _ -> False
                                        -- liftDebugIO (before, after, failed)
                                        return $ failed && (after == Right ())

addExpr :: String -> HP.HPage ()
addExpr e = HP.getPageText >>= \t -> let ne = t ++ "\n\n" ++ e in  HP.setPageText ne (length ne) >> return ()
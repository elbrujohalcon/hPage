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
-- import Utils.Log

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

newtype ClassName = CN {cnString :: String}
    deriving (Eq, Show)

instance Arbitrary ClassName where
    arbitrary = elements $ map CN [ "HPage", "IO", "IO a", "Int", "String"]
    coarbitrary _ = undefined

options :: TestOptions
options = TestOptions
      { no_of_tests         = 200
      , length_of_tests     = 1
      , debug_tests         = False }

main :: IO ()
main =
    do
        hps <- HPS.start
        hs <- HS.start
        runTests "HPage Server vs. Hint Server" options
                 [  run $ prop_fail hps hs
                 ,  run $ prop_eval hps hs
                 ,  run $ prop_typeOf hps hs
                 ,  run $ prop_kindOf hps hs
                 ,  run $ prop_load_module hps hs
                 ,  run $ prop_reload_modules hps hs
                 ]
        runTests "Cancelation" options
                 [  run $ prop_sequential hps
                 ,  run $ prop_cancel_load hps
                 ]
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
                 ,  run $ prop_find hps
                 ]
        runTests "Many Pages" options
                 [  run $ prop_new_page hps
                 ,  run $ prop_open_page hps
                 ,  run $ prop_open_page_fail hps
                 ,  run $ prop_setget_page hps
                 ,  run $ prop_setget_page_fail hps
                 ,  run $ prop_save_page hps
                 ,  run $ prop_save_page_as hps
                 ,  run $ prop_close_page hps
                 ,  run $ prop_is_modified_page hps
                 ,  run $ prop_is_modified_page_fail hps
                 ,  run $ prop_save_nth_page hps
                 ,  run $ prop_save_nth_page_fail hps
                 ,  run $ prop_save_nth_page_as hps
                 ,  run $ prop_save_nth_page_as_fail hps
                 ,  run $ prop_is_modified_nth_page hps
                 ,  run $ prop_is_modified_nth_page_fail hps
                 ,  run $ prop_close_nth_page hps
                 ,  run $ prop_close_nth_page_fail hps ]
                    

instance Eq (Hint.InterpreterError) where
    a == b = show a == show b

prop_eval :: HPS.ServerHandle -> HS.ServerHandle -> String -> Bool
prop_eval hps hs txt =
    unsafePerformIO $ do
                        let expr = "length \"" ++ txt ++ "\"" 
                        hpsr <- HPS.runIn hps $ HP.setText expr >> HP.eval
                        hsr <- HS.runIn hs $ Hint.eval expr
                        return $ hpsr == hsr

prop_typeOf :: HPS.ServerHandle -> HS.ServerHandle -> String -> Property
prop_typeOf hps hs txt = txt /= "" ==>
    unsafePerformIO $ do
                        let h = head txt
                        let expr = if isNumber h then [h, h] else "\"" ++ txt ++ "\""
                        hpsr <- HPS.runIn hps $ HP.setText expr >> HP.typeOf
                        hsr <- HS.runIn hs $ Hint.typeOf expr
                        return $ hpsr == hsr

prop_kindOf :: HPS.ServerHandle -> HS.ServerHandle -> ClassName -> Bool
prop_kindOf hps hs (CN expr) =
    unsafePerformIO $ do
                        hpsr <- HPS.runIn hps $ HP.setText expr >> HP.kindOf
                        hsr <- HS.runIn hs $ Hint.kindOf expr
                        return $ hpsr == hsr

prop_fail :: HPS.ServerHandle -> HS.ServerHandle -> String -> Bool
prop_fail hps hs txt =
    unsafePerformIO $ do
                        let expr = "lenggth \"" ++ txt ++ "\""
                        Left hpsr <- HPS.runIn hps $ HP.setText expr >> HP.eval
                        Left hsr <- HS.runIn hs $ Hint.eval expr
                        return $ hsr == hpsr
    
prop_load_module :: HPS.ServerHandle -> HS.ServerHandle -> String -> Bool
prop_load_module hps hs txt =
    unsafePerformIO $ do
                        let expr = "test = length \"" ++ txt ++ "\"" 
                        hpsr <- HPS.runIn hps $ do
                                                    HP.setText expr
                                                    HP.savePage "../documents/test.hs"
                                                    HP.setText "test"
                                                    HP.loadModule "../documents/test.hs"
                                                    HP.eval
                        hsr <- HS.runIn hs $ do
                                                Hint.loadModules ["../documents/test.hs"]
                                                Hint.getLoadedModules >>= Hint.setTopLevelModules
                                                Hint.eval "test"
                        return $ hpsr == hsr

prop_reload_modules :: HPS.ServerHandle -> HS.ServerHandle -> String -> Bool
prop_reload_modules hps hs txt =
    unsafePerformIO $ do
                        let expr = "test = show \"" ++ txt ++ "\"" 
                        hpsr <- HPS.runIn hps $ do
                                                    HP.setText expr
                                                    HP.savePage "../documents/test.hs"
                                                    HP.setText "test"
                                                    HP.loadModule "../documents/test.hs"
                                                    HP.reloadModules
                                                    HP.eval
                        hsr <- HS.runIn hs $ do
                                                Hint.loadModules ["../documents/test.hs"]
                                                Hint.getLoadedModules >>= Hint.setTopLevelModules
                                                Hint.eval "test"
                        return $ hpsr == hsr
    
prop_sequential :: HPS.ServerHandle -> String -> Bool
prop_sequential hps txt =
    unsafePerformIO $ do
                        let expr = "test = \"" ++ txt ++ "\""
                        HPS.runIn hps $ do
                                            HP.setText expr
                                            HP.savePage "../documents/test.hs"
                                            HP.loadModule' "../documents/test.hs"
                        Right hpsr <- HPS.runIn hps $ do
                                                        HP.setText "test"
                                                        HP.eval
                        return $ hpsr == show txt

prop_cancel_load :: HPS.ServerHandle -> ModuleName -> Bool
prop_cancel_load hps mn =
    unsafePerformIO $ do
                        let expr1 = "module " ++ show mn ++ " where fact = (1,2,3)"
                        let expr2 = "module " ++ show mn ++ "2 where fact = foldl (*) 1 [1.." ++ show (length $ show mn) ++ "]"
                        HPS.runIn hps $ do
                                            HP.reset
                                            HP.setText expr2
                                            HP.savePage $ "../documents/" ++ show mn ++ "2.hs"
                                            HP.setText expr1
                                            HP.savePage $ "../documents/" ++ show mn ++ ".hs"
                                            HP.setText "fact"
                                            HP.loadModule $ "../documents/" ++ show mn ++ ".hs"
                                            oldRes <- HP.eval
                                            HP.loadModule' $ "../documents/" ++ show mn ++ "2.hs"
                                            HP.cancel
                                            newRes <- HP.eval
                                            return $ newRes == oldRes

prop_setget_text :: HPS.ServerHandle -> String -> Bool
prop_setget_text hps txt =
    unsafePerformIO $ HPS.runIn hps $ do
                                        HP.setText txt
                                        HP.getText >>= return . (txt ==)

prop_setget_expr :: HPS.ServerHandle -> String -> Property
prop_setget_expr hps txt =
    txt /= "" ==>
    unsafePerformIO $ HPS.runIn hps $ do
                                        HP.setText $ txt ++ "\n\nxx"
                                        exi1 <- HP.getExprIndex
                                        exp1 <- HP.getExpr
                                        HP.setExprIndex 0
                                        exi0 <- HP.getExprIndex
                                        exp0 <- HP.getExpr
                                        HP.setExpr "yy"
                                        exi2 <- HP.getExprIndex
                                        exp2 <- HP.getExpr
                                        HP.setExprIndex 1
                                        exi3 <- HP.getExprIndex
                                        exp3 <- HP.getExpr
                                        -- liftDebugIO [(exi1, exp1), (exi0, exp0), (exi2, exp2), (exi3, exp3)]
                                        return $ (exi1 == 1) && (exp1 == "xx") &&
                                                 (exi0 == 0) && (exp0 == txt) &&
                                                 (exi2 == 0) && (exp2 == "yy") &&
                                                 (exi3 == 1) && (exp3 == "xx")

prop_setget_expr_fail :: HPS.ServerHandle -> String -> Bool
prop_setget_expr_fail hps _ =
    unsafePerformIO $ HPS.runIn hps $ try $ HP.clearPage >> HP.getExpr
    where try a = (a >> return False) `catchError` (\_ -> return True)

prop_addrem_expr :: HPS.ServerHandle -> String -> Property
prop_addrem_expr hps txt =
    txt /= "" ==>
    unsafePerformIO $ HPS.runIn hps $ do
                                        HP.clearPage
                                        HP.addExpr txt
                                        exi1 <- HP.getExprIndex
                                        exp1 <- HP.getExpr
                                        HP.removeExpr
                                        exi0 <- HP.getExprIndex
                                        exp0 <- HP.getText
                                        HP.addExpr txt
                                        HP.addExpr txt
                                        exi2 <- HP.getExprIndex
                                        exp2 <- HP.getExpr
                                        HP.removeExpr
                                        exi3 <- HP.getExprIndex
                                        exp3 <- HP.getText
                                        -- liftDebugIO [(exi1, exp1), (exi0, exp0), (exi2, exp2), (exi3, exp3)]
                                        return $ (exi1 == 0) && (exp1 == txt) &&
                                                 (exi0 == -1) && (exp0 == "") &&
                                                 (exi2 == 1) && (exp2 == txt) &&
                                                 (exi3 == 0) && (exp3 == txt)

prop_addrem_expr_fail :: HPS.ServerHandle -> String -> Bool
prop_addrem_expr_fail hps _ =
    unsafePerformIO $ HPS.runIn hps $ try $ HP.clearPage >> HP.removeExpr
    where try a = (a >> return False) `catchError` (\_ -> return True)

prop_setget_nth :: HPS.ServerHandle -> Int -> Property
prop_setget_nth hps i =
    i >= 0 ==>
    unsafePerformIO $ HPS.runIn hps $ do
                                        HP.clearPage
                                        replicateM (i+1) $ HP.addExpr "ww"
                                        exp0 <- HP.getNth 0
                                        exp1 <- HP.getNth i
                                        HP.setNth i "xxx"
                                        exp2 <- HP.getNth i
                                        HP.setNth i "x\n\ny"
                                        exp3 <- HP.getNth $ i+1
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
                                        set <- try $ HP.setNth i txt
                                        get <- try $ HP.getNth i
                                        -- liftDebugIO (get, set)
                                        return (set && get)
    where try a = (a >> return False) `catchError` (\_ -> return True)

prop_remove_nth :: HPS.ServerHandle -> Int -> Property
prop_remove_nth hps i =
    i > 0 ==>
    unsafePerformIO $ HPS.runIn hps $ do
                                        HP.clearPage
                                        forM [0..i] $ HP.addExpr . show
                                        HP.removeNth 0
                                        exp0 <- HP.getNth 0
                                        forM [i-2,i-3..0] $ HP.removeNth
                                        exp1 <- HP.getText
                                        -- liftDebugIO [exp0, exp1]
                                        return $ (exp0 == "1") &&
                                                 (exp1 == show i)

prop_remove_nth_fail :: HPS.ServerHandle -> Int -> Property
prop_remove_nth_fail hps i =
    i >= 0 ==>
    unsafePerformIO $ HPS.runIn hps $ HP.clearPage >> try (HP.removeNth $ i+1)
    where try a = (a >> return False) `catchError` (\_ -> return True)


prop_undoredo :: HPS.ServerHandle -> String -> Property
prop_undoredo hps txt =
    txt /= "" ==>
    unsafePerformIO $ HPS.runIn hps $ do
                                        HP.clearPage
                                        b0 <- HP.getText
                                        HP.setText txt
                                        b1 <- HP.getText
                                        HP.addExpr "xx"
                                        b2 <- HP.getText
                                        HP.addExpr "yy"
                                        b3 <- HP.getText
                                        HP.setExprIndex 1
                                        b4 <- HP.getText
                                        HP.setExpr $ txt ++ "|" ++ txt
                                        b5 <- HP.getText
                                        HP.removeExpr
                                        b6 <- HP.getText
                                        HP.removeNth 0
                                        b7 <- HP.getText
                                        HP.setNth 0 txt
                                        b8 <- HP.getText
                                        HP.addExpr "zz"
                                        let to10 = [0..10] :: [Int]
                                        after <- mapM (\_ -> HP.undo >> HP.getText) to10
                                        redo <- mapM (\_ -> HP.redo >> HP.getText) to10
                                        -- liftDebugIO [b8, b7, b6, b5, b4, b3, b2, b1, b0, "", ""]
                                        -- liftDebugIO after
                                        -- liftDebugIO [b0, b1, b2, b3, b4, b5, b6, b7, b8, b8, b8]
                                        -- liftDebugIO redo
                                        return $ ([b8, b7, b6, b5, b4, b3, b2, b1, b0, "", ""] == after) &&
                                                 ([b0, b1, b2, b3, b4, b5, b6, b7, b8, b8, b8] == redo)

prop_find :: HPS.ServerHandle -> Int -> Property
prop_find hps j =
    j > 0 ==>
    let i = (j+1) * 4 in
    unsafePerformIO $ HPS.runIn hps $ do
                                        HP.clearPage
                                        forM [1..i] $ HP.addExpr . (flip replicate) 'x'
                                        HP.setExprIndex (-1)
                                        HP.find "x"
                                        x0 <- HP.getExprIndex
                                        xexps <- mapM (\_ -> HP.findNext >> HP.getExprIndex) [1..i-1]
                                        HP.findNext
                                        x1 <- HP.getExprIndex
                                        forM [0,2..i-1] $ (\n -> HP.setNth n $ replicate (n+1) 'y')
                                        HP.setExprIndex (-1)
                                        HP.find "y"
                                        y0 <- HP.getExprIndex
                                        yexps <- mapM (\_ -> HP.findNext >> HP.getExprIndex) [1..i-1]
                                        HP.findNext
                                        y1 <- HP.getExprIndex
                                        HP.setExprIndex (-1)
                                        HP.find "z"
                                        z0 <- HP.getExprIndex
                                        
                                        -- liftDebugIO $ (x0, xexps, x1)
                                        -- liftDebugIO $ (y0, yexps, y1)
                                        -- liftDebugIO $ z0
                                        return $ x0 == 0 && xexps == [1..i-1] && x1 == 0 &&
                                                 y0 == 0 && yexps == ([2,4..i-1] ++ [0,2..i-1]) && y1 == 0 &&
                                                 z0 == (-1)

prop_new_page :: HPS.ServerHandle -> Int -> Property
prop_new_page _hps i = i > 0 ==> False

prop_open_page, prop_open_page_fail :: HPS.ServerHandle -> String -> Property
prop_open_page _hps file = file /= "" ==> False
prop_open_page_fail _hps file = file /= "" ==> False

prop_setget_page, prop_setget_page_fail :: HPS.ServerHandle -> Int -> Property
prop_setget_page _hps i = i > 0 ==> False
prop_setget_page_fail _hps i = i > 0 ==> False

prop_save_page, prop_save_page_as :: HPS.ServerHandle -> String -> Property
prop_save_page _hps file = file /= "" ==> False
prop_save_page_as _hps file = file /= "" ==> False

prop_is_modified_nth_page, prop_is_modified_nth_page_fail,
    prop_is_modified_page, prop_is_modified_page_fail,
    prop_save_nth_page, prop_save_nth_page_fail,
    prop_close_nth_page, prop_close_nth_page_fail,
    prop_close_page :: HPS.ServerHandle -> Int -> Property
prop_close_page _hps i = i > 0 ==> False
prop_is_modified_page _hps i = i > 0 ==> False
prop_is_modified_page_fail _hps i = i > 0 ==> False
prop_save_nth_page _hps i = i > 0 ==> False
prop_save_nth_page_fail _hps i = i > 0 ==> False
prop_is_modified_nth_page _hps i = i > 0 ==> False
prop_is_modified_nth_page_fail _hps i = i > 0 ==> False
prop_close_nth_page _hps i = i > 0 ==> False
prop_close_nth_page_fail _hps i = i > 0 ==> False

prop_save_nth_page_as, prop_save_nth_page_as_fail :: HPS.ServerHandle -> ModuleName -> Int -> Property
prop_save_nth_page_as _hps _file i = i > 0 ==> False
prop_save_nth_page_as_fail _hps _file i = i > 0 ==> False


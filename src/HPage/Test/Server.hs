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
import Utils.Log

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
{-
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
-}
        runTests "Editing" options
                 [  run $ prop_setget_text hps
                 ,  run $ prop_setget_expr hps
                 ,  run $ prop_setget_expr_fail hps
{-
                 ,  run $ prop_addrem_expr hps
                 ,  run $ prop_addrem_expr_fail hps
                 ,  run $ prop_setget_nth hps
                 ,  run $ prop_setget_nth_fail hps
                 ,  run $ prop_remove_nth hps
                 ,  run $ prop_remove_nth_fail hps
                 ,  run $ prop_undoredo hps
                 ,  run $ prop_find hps
-}
                 ]

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

{-
                 [  run $ prop_addrem_expr hps
                 ,  run $ prop_addrem_expr_fail hps
                 ,  run $ prop_setget_nth hps
                 ,  run $ prop_setget_nth_fail hps
                 ,  run $ prop_remove_nth hps
                 ,  run $ prop_remove_nth_fail hps
                 ,  run $ prop_undoredo hps
                 ,  run $ prop_find hps
-}
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
    unsafePerformIO $ catch (HPS.runIn hps $ HP.clearPage >> HP.getExpr >> return False)
                            (\_ -> return True)

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

instance Arbitrary Char where
    arbitrary = elements (['A'..'Z'] ++ ['a' .. 'z'] ++ " ~!@#$%^&*()")
    coarbitrary c = variant (ord c `rem` 16)

newtype ClassName = CN {asString :: String}
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
        quickCheck $ prop_cancel_load hps
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

prop_eval :: HPS.ServerHandle -> HS.ServerHandle -> String -> Bool
prop_eval hps hs txt =
    unsafePerformIO $ do
                        let expr = "length \"" ++ txt ++ "\"" 
                        hpsr <- HPS.runIn hps $ HP.setText expr >> HP.eval
                        Right hsr <- HS.runIn hs $ Hint.eval expr
                        return $ hpsr == hsr

prop_typeOf :: HPS.ServerHandle -> HS.ServerHandle -> String -> Property
prop_typeOf hps hs txt = txt /= "" ==>
    unsafePerformIO $ do
                        let h = head txt
                        let expr = if isNumber h then [h, h] else "\"" ++ txt ++ "\""
                        hpsr <- HPS.runIn hps $ HP.setText expr >> HP.typeOf
                        Right hsr <- HS.runIn hs $ Hint.typeOf expr
                        return $ hpsr == hsr

prop_kindOf :: HPS.ServerHandle -> HS.ServerHandle -> ClassName -> Bool
prop_kindOf hps hs (CN expr) =
    unsafePerformIO $ do
                        hpsr <- HPS.runIn hps $ HP.setText expr >> HP.kindOf
                        Right hsr <- HS.runIn hs $ Hint.eval expr
                        return $ hpsr == hsr

prop_fail :: HPS.ServerHandle -> HS.ServerHandle -> String -> Bool
prop_fail hps hs txt =
    unsafePerformIO $ do
                        let expr = "lenggth \"" ++ txt ++ "\""
                        Left hpsr <- HPS.runIn hps $ HP.setText expr >> try HP.eval
                        Left hsr <- HS.runIn hs $ Hint.eval expr
                        return $ "user error (" ++ show hsr ++ ")" == show hpsr
    where try a = (a >>= return . Right) `catchError` (return . Left)
    
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
                        Right hsr <- HS.runIn hs $ Hint.eval expr
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
                        Right hsr <- HS.runIn hs $ Hint.eval expr
                        return $ hpsr == hsr
    
prop_sequential :: HPS.ServerHandle -> String -> Bool
prop_sequential hps txt =
    unsafePerformIO $ do
                        let expr = "test = \"" ++ txt ++ "\""
                        HPS.runIn hps $ do
                                            HP.setText expr
                                            HP.savePage "../documents/test.hs"
                                            HP.loadModule' "../documents/test.hs"
                        hpsr <- HPS.runIn hps $ do
                                                    HP.setText "test"
                                                    HP.eval
                        return $ hpsr == show txt

prop_cancel_load :: HPS.ServerHandle -> String -> Bool
prop_cancel_load hps txt =
    unsafePerformIO $ do
                        let expr1 = "fact = (1,2,3)"
                        let expr2 = "fact = foldl (*) 1 [1.." ++ show (length txt) ++ "]"
                        HPS.runIn hps $ do
                                            HP.setText expr2
                                            HP.savePage "../documents/test2.hs"
                                            HP.setText expr1
                                            HP.savePage "../documents/test.hs"
                                            HP.setText "fact"
                                            HP.loadModule "../documents/test.hs"
                                            oldRes <- HP.eval
                                            HP.loadModule' "../documents/test2.hs"
                                            HP.cancel
                                            newRes <- HP.eval
                                            return $ newRes == oldRes

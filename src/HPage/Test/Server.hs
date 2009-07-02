
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
--        verboseCheck $ prop_async_one_at_a_time hps
        runTests "HPage Server vs. Hint Server" options
                 [  run $ prop_fail hps hs
                 ,  run $ prop_eval hps hs
                 ,  run $ prop_typeOf hps hs
                 ,  run $ prop_kindOf hps hs
                 ,  run $ prop_load_module hps hs
                 ,  run $ prop_reload_modules hps hs
                 ]
        runTests "Cancelation" options
                 [  run $ prop_async_one_at_a_time hps
                 ,  run $ prop_sync_one_at_a_time hps
                 ,  run $ prop_cancel_load hps
                 ]

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

prop_async_one_at_a_time :: HPS.ServerHandle -> String -> Bool
prop_async_one_at_a_time hps txt =
    unsafePerformIO $ do
                        let expr = "foldl (*) 1 [1.. ((length \"" ++ txt ++ "\") + 1)*10000]"
                        x <- try $ HPS.runIn hps $ HP.setText expr >> HP.eval' >> HP.eval'
                        case x of
                            Left _ ->
                                return True
                            Right _ ->
                                return False
    where try a = (a >>= return . Right) `catchError` (return . Left)
    
prop_sync_one_at_a_time :: HPS.ServerHandle -> String -> Bool
prop_sync_one_at_a_time hps txt =
    unsafePerformIO $ do
                        let expr = "length \"" ++ txt ++ "\""
                        HPS.runIn hps $ HP.setText expr >> HP.eval'
                        return . isBottom $ HPS.runIn hps $ return ()
    
prop_cancel_load :: HPS.ServerHandle -> String -> Bool
prop_cancel_load hps txt =
    unsafePerformIO $ do
                        let expr1 = "fact = (1,2,3)"
                        oldType <- HPS.runIn hps $ setFact expr1 >> HP.typeOf
                        let expr2 = "fact = foldl (*) 1 [1.." ++ show (length txt) ++ "]"
                        HPS.runIn hps $ setFact expr2 >> HP.typeOf'
                        HPS.runIn hps HP.cancel
                        newType <- HPS.runIn hps $ HP.setText "fact" >> HP.typeOf
                        return $ newType == oldType
    where setFact expr = do
                            HP.setText expr
                            HP.savePage "../documents/test.hs"
                            HP.loadModule "../documents/test.hs"
                            HP.setText "fact"

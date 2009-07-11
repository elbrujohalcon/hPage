
module HPage.Test.Main where

import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Server as HS

main :: IO ()
main = do
            hs1 <- HS.start
            hs2 <- HS.start
            let test1 = "../documents/Testx.hs"
            let test2 = "../documents/Testy.hs"
            let expr = "test"
            writeFile test1 "test = (1,2,3)"
            writeFile test2 "test = (3,2,1)"
            let load f = do
                            Hint.loadModules [f]
                            ms <- Hint.getLoadedModules
                            Hint.setTopLevelModules ms
            line "1" $ HS.runIn hs1 $ load test1
            line "2" $ HS.runIn hs2 $ load test2
            line "3" $ HS.runIn hs1 $ Hint.eval expr
            line "4" $ HS.runIn hs2 $ Hint.eval expr
    where line n a = a >>= putStrLn . (n ++) . (' ':) . show

module Main where

--import Graphics.UI.WX
import Control.Monad.Trans
--import HPage.GUI.MainWindow
import HPage.Control

--main :: IO ()
--main = start mainWindow

main :: IO ()
main = evalHPage hpage

hpage :: HPage ()
hpage = do
            setText "let x = 1 in\n\tx+x\n\npepepe"
            res <- evalFirst
            case res of
                Left e ->
                    liftIO $ putStrLn $ show e
                Right v ->
                    liftIO $ putStrLn $ show v
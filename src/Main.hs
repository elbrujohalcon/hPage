module Main where

import Graphics.UI.WX
import HPage.GUI.MainWindow
import System.Environment

main :: IO ()
main = getArgs >>= start . gui . filter (\arg -> head arg /= '-')
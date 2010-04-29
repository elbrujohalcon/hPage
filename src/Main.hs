module Main where

import Graphics.UI.WX
import HPage.GUI.FreeTextWindow
import System.Environment

main :: IO ()
main = getArgs >>= start . gui
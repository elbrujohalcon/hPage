
module HPage.GUI.SplashScreen (start, step, SplashHandle) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.Process
import Data.Bits
import Graphics.UI.WX hiding (start)
import Graphics.UI.WXCore hiding (kill)
import System.FilePath
import System.Environment.FindBin
import Paths_hpage
import HPage.Utils.Log

imageFile :: IO FilePath
imageFile = do
                progPath <- getProgPath
                case takeBaseName progPath of
                    "MacOS" ->
                        return $ dropFileName progPath </> "Resources" </> "splash.png"
                    _ ->
                        getDataFileName $ "res" </> "images" </> "splash" <.> "png"
                                
newtype SplashHandle = SH {handle :: Handle (Int, String)}

start :: Window a -> IO SplashHandle
start topWin = (spawn $ guiRunner topWin) >>= return . SH
  where guiRunner tw = do
            (caption, progress, win) <- liftIO $ gui tw
            forever $ do
                        myself <- self
                        (percent, lbl) <- recv
                        liftIO $ case percent of
                                     100 ->
                                         do
                                             debugIO "closing the window"
                                             set win [visible := False]
                                             True <- windowDestroy win
                                             kill myself 
                                     _ ->
                                         do
                                             debugIO ("progress", percent, lbl)
                                             set caption [text := lbl]
                                             set progress [selection := percent]
                                             _int <- wxcAppSafeYield win
                                             return ()

step :: SplashHandle -> Int -> String -> IO ()
step sh i s =
    do
        debugIO ("step", i, s)
        sendTo (handle sh) (i,s)

gui :: Window a -> IO (StaticText (), Gauge (), Frame ())
gui topWin =
    do
        let frameStyle = wxSTAY_ON_TOP .&. complement (wxRESIZE_BORDER .|.
                                                       wxMAXIMIZE_BOX .|.
                                                       wxMINIMIZE_BOX .|.
                                                       wxSYSTEM_MENU .|.
                                                       wxCLOSE_BOX .|.
                                                       wxCAPTION)
        win <- frameEx frameStyle [] topWin
        _timer <- timer win [interval := 5, on command := putStr ":"]
        
        img <- imageFile
        htmlw <- htmlWindowCreate win idAny (rect (point 0 0) (sz 870 176)) wxHW_SCROLLBAR_NEVER ""
        htmlWindowSetBorders htmlw 0
        htmlWindowSetPage htmlw $ "<img src='" ++ img ++ "' />"
        
        caption <- staticText win [text := "Loading..."]
        
        progress <- hgauge win 100 [selection := 0]
        
        set win [layout := column 0 [hfloatCenter $ widget htmlw,
                                     hfloatCenter $ widget caption,
                                     hfill $ widget progress],
                 visible := True,
                 clientSize := sz 870 200]
        windowCenter win wxCENTRE_ON_SCREEN
        
        return (caption, progress, win)
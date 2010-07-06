import Distribution.MacOSX
import Distribution.Simple
import System.Cmd
import System.Exit
import System.Directory
import System.FilePath
import System.Info (os)
import Control.Monad

import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.LocalBuildInfo

main :: IO ()
main = do
         resources <- getAllDirectoryContents "res"
         let usefulres = flip filter resources $ \r -> r /= ("res" </> "images" </> "icon" </> "hpage.icns")
         defaultMainWithHooks $ simpleUserHooks {
                postBuild = appBundleBuildHook $ [guiApp usefulres], -- no-op if not MacOS X
                postInst = appBundleInstall $ guiApp usefulres,
                runTests = runTests'
         }

guiApp :: [FilePath] -> MacApp
guiApp rs = MacApp "hpage"
                  (Just $ "res" </> "images" </> "icon" </> "hpage.icns")
                  Nothing -- Build a default Info.plist for the icon.
                  rs
                  [] -- No other binaries.
                  (ChaseWith $ "libstdc++." : defaultExclusions)
          
getAllDirectoryContents :: FilePath -> IO [FilePath]
getAllDirectoryContents p =
  do
    allContents <- getDirectoryContents p
    let nonHidden = map (\path -> p </> path) $ flip filter allContents $ \path -> head path /= '.'
    recContents <- forM nonHidden $ \path -> do
                                        exists <- doesDirectoryExist path
                                        if exists
                                          then getAllDirectoryContents path
                                          else return [path]
    return $ concat recContents

appBundleInstall :: MacApp -> Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
appBundleInstall app _ _ pkg localb =
    case os of
        "darwin" ->
            do
                ExitSuccess <- copyDirectory appPath theBindir
                writeFile scriptFile scriptText
        _ ->
            return ()
    where
        theBindir = bindir $ absoluteInstallDirs pkg localb NoCopyDest
        appPath = buildDir localb </> appName app <.> "app"
        scriptFile = theBindir </> appName app
        finalAppPath = theBindir </> takeFileName appPath 
        scriptText = finalAppPath </> "Contents/MacOS" </> appName app ++ " \"$@\""
        -- scriptText = "open " ++ finalAppPath
        
copyDirectory :: FilePath -> FilePath -> IO ExitCode
copyDirectory dir newLocation = rawSystem "cp" ["-rf", dir, newLocation]

runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTests' _ _ _ lbi = do
          built <- doesDirectoryExist $ buildDir lbi
          unless built $ die "Run the 'build' command first."
          system $ "cd src && runhaskell -i../dist/build/autogen HPage.Test.Server && cd .."
          return ()
import Distribution.MacOSX
import Distribution.Simple
import System.Directory
import System.FilePath
import Control.Monad

main :: IO ()
main = do
         resources <- getAllDirectoryContents "res"
         let usefulres = flip filter resources $ \r -> r /= ("res" </> "images" </> "icon" </> "hpage.icns")
         putStrLn "Resources: "
         forM_ usefulres $ putStrLn . ('\t':)
         defaultMainWithHooks $ simpleUserHooks {
                postBuild = appBundleBuildHook $ guiApps usefulres -- no-op if not MacOS X
         }

guiApps :: [FilePath] -> [MacApp]
guiApps rs = [MacApp "hpage"
                  (Just $ "res" </> "images" </> "icon" </> "hpage.icns")
                  Nothing -- Build a default Info.plist for the icon.
                  rs
                  [] -- No other binaries.
                  ChaseWithDefaults
             ]
          
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
                                        
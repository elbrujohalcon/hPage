{-# LANGUAGE CPP #-}

import Control.Monad (foldM_, forM_)
import System.Cmd
import System.Exit
import System.Info (os)
import System.FilePath
import System.Directory ( doesFileExist, copyFile, removeFile, createDirectoryIfMissing )

import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo

#ifndef WIN32
import System.Posix.Files (fileMode, getFileStatus, setFileMode,
                           ownerExecuteMode, groupExecuteMode, otherExecuteMode)
import Data.Bits ( (.|.) )
#endif

main :: IO ()
main = do
            putStrLn $ "Setting up hpage for " ++ os
            defaultMainWithHooks $ addMacHook simpleUserHooks
 where
  addMacHook h =
   case os of
    "darwin" -> h { postInst = appBundleHook,
                    runTests = hPageTestRunner } -- is it OK to treat darwin as synonymous with MacOS X?
    _        -> h { runTests = hPageTestRunner }

appBundleHook :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
appBundleHook _ _ pkg localb =
 forM_ exes $ \app ->
   do createAppBundle theBindir (buildDir localb </> app </> app)
      removeFile (theBindir </> app)
      createAppBundleWrapper theBindir app
 where
  theBindir = bindir $ absoluteInstallDirs pkg localb NoCopyDest
  exes = map exeName $ executables pkg

-- ----------------------------------------------------------------------
-- helper code for application bundles
-- ----------------------------------------------------------------------

-- | 'createAppBundle' @d p@ - creates an application bundle in @d@
--   for program @p@, assuming that @d@ already exists and is a directory.
--   Note that only the filename part of @p@ is used.
createAppBundle :: FilePath -> FilePath -> IO ()
createAppBundle dir p =
 do createDirectoryIfMissing False $ bundle
    createDirectoryIfMissing True  $ bundleBin
    createDirectoryIfMissing True  $ bundleRsrc
    copyFile p (bundleBin </> takeFileName p)
 where
  bundle     = appBundlePath dir p
  bundleBin  = bundle </> "Contents/MacOS"
  bundleRsrc = bundle </> "Contents/Resources"

-- | 'createAppBundleWrapper' @d p@ - creates a script in @d@ that calls
--   @p@ from the application bundle @d </> takeFileName p <.> "app"@
createAppBundleWrapper :: FilePath -> FilePath -> IO ()
createAppBundleWrapper bindir p =
  do writeFile scriptFile scriptTxt
     makeExecutable scriptFile
 where
  scriptFile = bindir </> takeFileName p
  scriptTxt = "`dirname $0`" </> appBundlePath "." p </> "Contents/MacOS" </> takeFileName p ++ " \"$@\""

appBundlePath :: FilePath -> FilePath -> FilePath
appBundlePath dir p = dir </> takeFileName p <.> "app"

-- ----------------------------------------------------------------------
-- utilities
-- ----------------------------------------------------------------------

makeExecutable :: FilePath -> IO ()
#ifdef WIN32
makeExecutable = const (return ())
#else
makeExecutable f =
  do st <- getFileStatus f
     let m  = fileMode st
         m2 = m .|. ownerExecuteMode .|. groupExecuteMode .|. otherExecuteMode
     setFileMode f m2
#endif

hPageTestRunner :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
hPageTestRunner _ _ _ _ = do
                            system "runhaskell -i./src src/HPage/Test/Server.hs"
                            return ()
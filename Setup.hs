import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription

import System.IO
import System.Exit
import System.Process
import Control.Monad
import System.FilePath
import System.Directory


main :: IO ()
main = defaultMainWithHooks simpleUserHooks { postBuild = myPostBuild }

myPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostBuild args flags pd lbi =
    do putStrLn "Custom build step: compiling debuggerInterface.elm"
       buildInterface lbi
       postBuild simpleUserHooks args flags pd lbi

buildInterface :: LocalBuildInfo -> IO ()
buildInterface lbi =
    do exitCode <- compile $ args "slider" "debuggerInterface.elm" "assets"
       case exitCode of
            ExitFailure _ ->
                putStrLn "Build failed: debuggerInterface"
            ExitSuccess ->
                do handle <- runCommand "mv assets/slider/debuggerInterface.js assets/"
                   mvExit <- waitForProcess handle
                   return ()
       removeEverything "slider" "Slider.elm"
       removeEverything "slider" "debuggerInterface.elm"
    where
        args indir infile outdir =
            [ "--make"
            , "--only-js"
            , "--build-dir=" ++ outdir
            , "--src-dir=" ++ indir
            , indir </> infile
            ]

        compile args =
            do handle <- runProcess "elm" args Nothing Nothing Nothing Nothing Nothing
               exitCode <- waitForProcess handle
               return exitCode

        removeEverything subdir file =
            do remove "cache" "elmi"
               remove "cache" "elmo"
               remove "build" "js"
            where
              remove :: String -> String -> IO ()
              remove dir ext = do
                let path = dir </> subdir </> file`replaceExtension` ext
                exists <- doesFileExist path
                when exists (removeFile path)

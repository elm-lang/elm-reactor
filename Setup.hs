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
    do putStrLn "Custom build step: compiling sliderInterface.elm and watchesInterface.elm"
       buildSliderInterface lbi
       buildWatchesInterface lbi
       concatJS lbi
       postBuild simpleUserHooks args flags pd lbi

concatJS :: LocalBuildInfo -> IO ()
concatJS lbi =
  do let files = map (("assets" </> "_reactor") </>)
          [ "watchesInterface.js"
          , "sliderInterface.js"
          , "toString.js"
          , "core.js"
          , "reactor.js"
          ]
     megaJS <- concat `fmap` mapM readFile files
     _ <- putStrLn "Writing composite debugger.js"
     writeFile ("assets" </> "debugger.js") megaJS

buildSliderInterface :: LocalBuildInfo -> IO ()
buildSliderInterface lbi =
    do exitCode <- compile $ args "sliderInterface.elm"
       case exitCode of
            ExitFailure _ ->
                putStrLn "Build failed: sliderInterface"
            ExitSuccess ->
                renameFile
                    ("slider" </> "build" </> "sliderInterface.js")
                    ("assets" </> "_reactor" </> "sliderInterface.js")

       removeEverything "slider" "Slider.elm"
       removeEverything "slider" "sliderInterface.elm"
    where
        args file =
            [ "--make"
            , "--only-js"
            , file
            ]

        compile args =
            do let workingDir = Just "slider"
               handle <- runProcess "elm" args workingDir Nothing Nothing Nothing Nothing
               exitCode <- waitForProcess handle
               return exitCode

        removeEverything dir file =
            do remove "cache" "elmi"
               remove "cache" "elmo"
               remove "build" "js"
            where
                remove :: String -> String -> IO ()
                remove subdir ext =
                    do let path = dir </> subdir </> file`replaceExtension` ext
                       exists <- doesFileExist path
                       when exists (removeFile path)

buildWatchesInterface :: LocalBuildInfo -> IO ()
buildWatchesInterface lbi =
    do exitCode <- compile $ args "watchesInterface.elm"
       case exitCode of
            ExitFailure _ ->
                putStrLn "Build failed: watchesInterface"
            ExitSuccess ->
                renameFile
                    ("slider" </> "build" </> "watchesInterface.js")
                    ("assets" </> "_reactor" </> "watchesInterface.js")

       removeEverything "slider" "watchesInterface.elm"
    where
        args file =
            [ "--make"
            , "--only-js"
            , file
            ]

        compile args =
            do let workingDir = Just "slider"
               handle <- runProcess "elm" args workingDir Nothing Nothing Nothing Nothing
               exitCode <- waitForProcess handle
               return exitCode

        removeEverything dir file =
            do remove "cache" "elmi"
               remove "cache" "elmo"
               remove "build" "js"
            where
                remove :: String -> String -> IO ()
                remove subdir ext =
                    do let path = dir </> subdir </> file`replaceExtension` ext
                       exists <- doesFileExist path
                       when exists (removeFile path)

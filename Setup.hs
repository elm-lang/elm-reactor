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
       concatJS lbi
       postBuild simpleUserHooks args flags pd lbi

concatJS :: LocalBuildInfo -> IO ()
concatJS lbi =
  do let files = map ("assets" </>)
          [ "_reactor" </> "debuggerInterface.js"
          , "_reactor" </> "toString.js"
          , "debug-core.js"
          ]
     megaJS <- concat `fmap` mapM readFile files
     _ <- putStrLn "Writing composite debugger.js"
     writeFile ("assets" </> "debugger.js") megaJS

buildInterface :: LocalBuildInfo -> IO ()
buildInterface lbi =
    do exitCode <- compile $ args "debuggerInterface.elm"
       case exitCode of
            ExitFailure _ ->
                putStrLn "Build failed: debuggerInterface"
            ExitSuccess ->
                do handle <- runCommand "mv slider/build/debuggerInterface.js assets/_reactor/"
                   mvExit <- waitForProcess handle
                   return ()
       removeEverything "slider" "Slider.elm"
       removeEverything "slider" "debuggerInterface.elm"
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

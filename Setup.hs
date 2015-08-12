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


-- RUN EVERYTHING

main :: IO ()
main =
  defaultMainWithHooks simpleUserHooks { postBuild = myPostBuild }


myPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostBuild args flags pd lbi =
  do  putStrLn "Custom build step: creating and collecting all static resources"

      buildSideBar
      buildIndex

      src <- readFile ("frontend" </> "debugger-implementation.js")
      appendFile debugPath src
      postBuild simpleUserHooks args flags pd lbi


-- PATHS

debugPath :: FilePath
debugPath =
  "assets" </> "_reactor" </> "debug.js"


indexPath :: FilePath
indexPath =
  "assets" </> "_reactor" </> "index.js"


-- BUILD STATIC FILES

buildSideBar :: IO ()
buildSideBar =
  compile "Debugger.elm" debugPath


buildIndex :: IO ()
buildIndex =
  compile "Index.elm" indexPath


compile :: FilePath -> FilePath -> IO ()
compile source target =
  do  (exitCode, out, err) <-
          readProcessWithExitCode
              "elm-make"
              [ "--yes", "frontend" </> source, "--output=" ++ target ]
              ""

      case exitCode of
        ExitSuccess ->
          return ()

        ExitFailure _ ->
          do  hPutStrLn stderr (unlines ["Failed to build" ++ source, "", out, err])
              exitFailure



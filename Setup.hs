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
main =
  defaultMainWithHooks simpleUserHooks { postBuild = myPostBuild }


myPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostBuild args flags pd lbi =
  do  putStrLn "Custom build step: creating and collecting all static resources"
      buildSideBar
      append "debug.js"
      append "toString.js"
      append "core.js"
      append "reactor.js"
      postBuild simpleUserHooks args flags pd lbi


append :: FilePath -> IO ()
append fileName =
  do  src <- readFile ("frontend" </> fileName)
      appendFile output src


output :: FilePath
output = "assets" </> "_reactor" </> "debug.js"


buildSideBar :: IO ()
buildSideBar =
  do  (exitCode, out, err) <-
        readProcessWithExitCode "elm-make" [ "--yes", "frontend" </> "SideBar.elm", "--output=" ++ output ] ""
      case exitCode of
        ExitSuccess ->
          return ()

        ExitFailure _ ->
          do  hPutStrLn stderr ("Failed to build SideBar.elm\n\n" ++ out ++ err)
              exitFailure

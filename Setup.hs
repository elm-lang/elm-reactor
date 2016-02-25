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
import GHC.IO.Encoding (setLocaleEncoding, utf8)


main :: IO ()
main =
  do  setLocaleEncoding utf8
      defaultMainWithHooks simpleUserHooks { postBuild = myPostBuild }


myPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostBuild args flags pd lbi =
  do  putStrLn "Custom build step: creating and collecting all static resources"
      buildSideBar
      src <- readFile ("frontend" </> "debugger-implementation.js")
      appendFile output src
      postBuild simpleUserHooks args flags pd lbi


output :: FilePath
output =
  "assets" </> "_reactor" </> "debug.js"


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

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


debugScriptPath :: FilePath
debugScriptPath = outputPath "debug.js"


indexScriptPath :: FilePath
indexScriptPath = outputPath "index.js"


toBuild :: [IO ()]
toBuild =
  [ buildSideBar
  , buildIndex
  ]


outputPath :: FilePath -> FilePath
outputPath = (("assets" </> "_reactor") </>)


elmCompile :: FilePath -> FilePath -> IO ()
elmCompile source target = do
  (exitCode, out, err) <- readProcessWithExitCode
                            "elm-make"
                            [ "--yes", "frontend" </> source, "--output=" ++ target ]
                            ""
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure _ ->
      hPutStrLn stderr (unlines ["Failed to build" ++ source, "", out, err]) >>
      exitFailure


main :: IO ()
main =
  defaultMainWithHooks simpleUserHooks { postBuild = myPostBuild }


myPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostBuild args flags pd lbi =
  do  putStrLn "Custom build step: creating and collecting all static resources"
      sequence_ toBuild
      src <- readFile ("frontend" </> "debugger-implementation.js")
      appendFile debugScriptPath src
      postBuild simpleUserHooks args flags pd lbi


buildSideBar :: IO ()
buildSideBar = elmCompile "SideBar.elm" debugScriptPath


buildIndex :: IO ()
buildIndex = elmCompile "Index.elm" indexScriptPath

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
  do  putStrLn "Custom build step: compiling debuggerInterface.elm"
      buildInterface
      concatJS lbi
      postBuild simpleUserHooks args flags pd lbi


concatJS :: LocalBuildInfo -> IO ()
concatJS lbi =
  do  megaJS <- concat `fmap` mapM readFile jsFiles
      _ <- putStrLn "Writing composite debugger.js"
      writeFile ("assets" </> "debugger.js") megaJS


jsFiles :: [FilePath]
jsFiles =
  map (\name -> "assets" </> "_reactor" </> name)
    [ "debuggerInterface.js"
    , "toString.js"
    , "core.js"
    , "reactor.js"
    ]


buildInterface :: IO ()
buildInterface =
  do  (exitCode, out, err) <-
        readProcessWithExitCode "elm-make" [ "--yes", "frontend" </> "debuggerInterface.elm" ] ""
      case exitCode of
        ExitSuccess ->
          renameFile "elm.js" ("assets" </> "_reactor" </> "debuggerInterface.js")

        ExitFailure _ ->
          do  hPutStrLn stderr ("Failed to build debuggerInterface.elm\n\n" ++ out ++ err)
              exitFailure

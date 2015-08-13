{-# OPTIONS_GHC -Wall #-}
module StaticFiles.Build (debugger, navigationPage) where

import qualified Data.ByteString as BS
import System.Directory (removeFile)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), replaceExtension)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)


debugger :: IO BS.ByteString
debugger =
  let
    tempFile =
      "temp-debug.js"
  in
    do  compile "Debugger.elm" tempFile
        part1 <- BS.readFile tempFile
        part2 <- BS.readFile ("frontend" </> "debugger-implementation.js")
        let result = BS.concat [ part1, part2 ]
        seq (BS.length result) (removeFile tempFile)
        return result


navigationPage :: FilePath -> IO BS.ByteString
navigationPage fileName =
  let
    tempFile =
      "temp-" ++ replaceExtension fileName "js"
  in
    do  compile fileName tempFile
        result <- BS.readFile tempFile
        seq (BS.length result) (removeFile tempFile)
        return result


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

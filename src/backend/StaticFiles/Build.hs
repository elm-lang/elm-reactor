{-# OPTIONS_GHC -Wall #-}
module StaticFiles.Build
    ( debuggerAgent, debuggerInterfaceJs, debuggerInterfaceHtml
    , navigationPage, favicon
    ) where

import qualified Data.ByteString as BS
import System.Directory (removeFile)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), replaceExtension)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)



-- READ STATIC FILES


debuggerAgent :: IO BS.ByteString
debuggerAgent =
  BS.readFile ("frontend" </> "debug-agent.js")


debuggerInterfaceHtml :: IO BS.ByteString
debuggerInterfaceHtml =
  BS.readFile ("frontend" </> "debug-interface.html")


favicon :: IO BS.ByteString
favicon =
  BS.readFile ("assets" </> "favicon.ico")



-- COMPILE ELM CODE


navigationPage :: FilePath -> IO BS.ByteString
navigationPage fileName =
  let
    tempFile =
      "temp-" ++ replaceExtension fileName "js"
  in
    do  compile ("src" </> "pages" </> fileName) tempFile
        result <- BS.readFile tempFile
        seq (BS.length result) (removeFile tempFile)
        return result


compile :: FilePath -> FilePath -> IO ()
compile source target =
  do  (exitCode, out, err) <-
          readProcessWithExitCode
              "elm-make"
              [ "--yes", source, "--output=" ++ target ]
              ""

      case exitCode of
        ExitSuccess ->
          return ()

        ExitFailure _ ->
          do  hPutStrLn stderr (unlines ["Failed to build" ++ source, "", out, err])
              exitFailure


debuggerInterfaceJs :: IO BS.ByteString
debuggerInterfaceJs =
  let
    tempFile =
      "temp-debug.js"
  in
    do  compile ("frontend" </> "Debugger.elm") tempFile
        result <- BS.readFile tempFile
        seq (BS.length result) (removeFile tempFile)
        return result

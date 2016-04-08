{-# OPTIONS_GHC -Wall #-}
module StaticFiles.Build
    ( favicon, waiting
    , compile
    )
    where

import qualified Data.ByteString as BS
import System.Directory (removeFile)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), (<.>), takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)



-- READ STATIC FILES


favicon :: IO BS.ByteString
favicon =
  BS.readFile ("assets" </> "favicon.ico")


waiting :: IO BS.ByteString
waiting =
  BS.readFile ("assets" </> "waiting.gif")



-- COMPILE TO JS


compile :: FilePath -> IO BS.ByteString
compile fileName =
  let
    tempFile =
      "temp-orary-" ++ takeBaseName fileName <.> "js"
  in
    do  elmMake fileName tempFile
        result <- BS.readFile tempFile
        seq (BS.length result) (removeFile tempFile)
        return result


elmMake :: FilePath -> FilePath -> IO ()
elmMake source target =
  do  (exitCode, out, err) <-
          readProcessWithExitCode
              "elm-make"
              [ "--yes", source, "--output=" ++ target ]
              ""

      case exitCode of
        ExitSuccess ->
          return ()

        ExitFailure _ ->
          do  hPutStrLn stderr (unlines ["Failed to build " ++ source, "", out, err])
              exitFailure

{-# OPTIONS_GHC -Wall #-}
module StaticFiles.Build
    ( compile
    )
    where

import qualified Data.ByteString as BS
import Data.ByteString.Char8 (unpack)
import System.Directory (removeFile)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((<.>), takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process.ByteString (readProcessWithExitCode)



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
              BS.empty

      case exitCode of
        ExitSuccess ->
          return ()

        ExitFailure _ ->
          do  hPutStrLn stderr (unlines ["Failed to build " ++ source, "",
                                         unpack out, unpack err])
              exitFailure

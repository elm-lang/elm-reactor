{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module StaticFiles
    ( errors
    , index, indexPath
    , notFound, notFoundPath
    , favicon, faviconPath
    , waiting, waitingPath
    )
    where

import qualified Data.ByteString as BS
import Data.FileEmbed (bsToExp)
import Language.Haskell.TH (runIO)
import System.FilePath ((</>))

import qualified StaticFiles.Build as Build



-- PATHS


faviconPath :: FilePath
faviconPath =
  "favicon.ico"


waitingPath :: FilePath
waitingPath =
  "_reactor/waiting.gif"


indexPath :: FilePath
indexPath =
  "_reactor/index.js"


notFoundPath :: FilePath
notFoundPath =
  "_reactor/notFound.js"



-- RAW RESOURCES


errors :: BS.ByteString
errors =
  $(bsToExp =<< runIO (Build.compile ("src" </> "pages" </> "Errors.elm")))


index :: BS.ByteString
index =
  $(bsToExp =<< runIO (Build.compile ("src" </> "pages" </> "Index.elm")))


notFound :: BS.ByteString
notFound =
  $(bsToExp =<< runIO (Build.compile ("src" </> "pages" </> "NotFound.elm")))


favicon :: BS.ByteString
favicon =
  $(bsToExp =<< runIO (BS.readFile ("assets" </> "favicon.ico")))


waiting :: BS.ByteString
waiting =
  $(bsToExp =<< runIO (BS.readFile ("assets" </> "waiting.gif")))


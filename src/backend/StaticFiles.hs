{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module StaticFiles
    ( index, indexPath
    , notFound, notFoundPath
    , favicon, faviconPath
    )
    where

import qualified Data.ByteString as BS
import Data.FileEmbed (bsToExp)
import Language.Haskell.TH (runIO)

import qualified StaticFiles.Build as Build



-- PATHS


faviconPath :: FilePath
faviconPath =
  "_reactor/favicon.ico"


indexPath :: FilePath
indexPath =
  "_reactor/index.js"


notFoundPath :: FilePath
notFoundPath =
  "_reactor/notFound.js"



-- RAW RESOURCES


index :: BS.ByteString
index =
  $(bsToExp =<< runIO (Build.navigationPage "Index.elm"))


notFound :: BS.ByteString
notFound =
  $(bsToExp =<< runIO (Build.navigationPage "NotFound.elm"))


favicon :: BS.ByteString
favicon =
  $(bsToExp =<< runIO Build.favicon)


module Generate.NotFound where

import qualified Data.ByteString.Char8 as BSC

import qualified StaticFiles
import qualified Generate.Help as Help


html :: BSC.ByteString
html =
  Help.makeHtml
    "Page Not Found"
    ("/" ++ StaticFiles.notFoundPath)
    "Elm.NotFound.fullscreen();"

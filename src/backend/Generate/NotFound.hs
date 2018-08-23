module Generate.NotFound where

import qualified Text.Blaze.Html5 as H

import qualified StaticFiles
import qualified Generate.Help as Help


html :: H.Html
html =
  Help.makeHtml
    "Page Not Found"
    ("/" ++ StaticFiles.notFoundPath)
    "Elm.NotFound.fullscreen();"
    []

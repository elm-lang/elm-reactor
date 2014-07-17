{-# LANGUAGE OverloadedStrings #-}
module Debugger (ide) where

import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import Network.HTTP.Base (urlEncode)
import qualified System.FilePath as FP

-- | Display the elm program and debugging controls
ide :: FilePath -> Html
ide filePath =
    let (directory, fileName) = FP.splitFileName filePath
    in  ideBuilder ("Elm Debugger: " ++ FP.takeBaseName fileName)
               ("/" ++ directory ++ urlEncode fileName ++ "?debug=true")


ideBuilder :: String -> String -> Html
ideBuilder title output =
    H.docTypeHtml $ do
      H.head $ do
        H.title . toHtml $ title
      preEscapedToMarkup $
         concat [ "  <frameset cols=\"220,*\" frameborder=\"0\">\n"
                , "    <frame name=\"debug\" src=\"/elm-debugger.html\" />\n"
                , "    <frame name=\"output\" src=\"", output, "\" />\n"
                , "  </frameset>"
                ]


{-# LANGUAGE OverloadedStrings #-}
module Editor (ide) where

import Data.Monoid (mempty)
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Network.HTTP.Base (urlEncode)
import qualified System.FilePath as FP

-- | Display the elm program and debugging controls
ide :: FilePath -> Html
ide fileName =
    ideBuilder ("Elm Debugger: " ++ FP.takeBaseName fileName)
               fileName
               ("/compile?input=" ++ urlEncode fileName)


ideBuilder :: String -> String -> String -> Html
ideBuilder title input output =
    H.docTypeHtml $ do
      H.head $ do
        H.title . toHtml $ title
      preEscapedToMarkup $
         concat [ "  <frameset rows=\"*,110\">\n"
                , "    <frame name=\"output\" src=\"", output, "\" />\n"
                , "    <frame name=\"debug\" src=\"/debugger/elm-debugger.html\" />\n"
                , "  </frameset>"
                ]


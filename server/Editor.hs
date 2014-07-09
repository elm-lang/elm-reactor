{-# LANGUAGE OverloadedStrings #-}
module Editor (ide,empty) where

import Data.Monoid (mempty)
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Network.HTTP.Base (urlEncode)
import qualified System.FilePath as FP

-- | Display an editor and the compiled result side-by-side.
ide :: FilePath -> String -> Html
ide fileName code =
    ideBuilder ("Elm Debugger: " ++ FP.takeBaseName fileName)
               fileName
               ("/compile?input=" ++ urlEncode code)

-- | Display an editor and the compiled result side-by-side.
empty :: Html
empty = ideBuilder "Debug Elm" "Empty.elm" "/Try.elm"

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


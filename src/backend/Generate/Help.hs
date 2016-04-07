{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.Help (makeHtml, makeCodeHtml) where

import qualified Data.ByteString.Char8 as BSC
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Highlighting.Kate as Kate



-- PAGES


makeHtml :: String -> String -> String -> BSC.ByteString
makeHtml title jsFile initCode =
  BSC.pack $ unlines $
    [ "<html>"
    , ""
    , "<head>"
    , "  <title>" ++ title ++ "</title>"
    , "  <link rel='icon' href='/_reactor/favicon.ico' sizes='32x32'>"
    , "  <script src=\"" ++ jsFile ++ "\"></script>"
    , "  <style>"
    , "    @import url(http://fonts.googleapis.com/css?family=Source+Sans+Pro:400,700,400italic,700italic|Source+Code+Pro);"
    , "    html, head, body {"
    , "      margin: 0;"
    , "      height: 100%;"
    , "    }"
    , "    body {"
    , "      font-family: 'Source Sans Pro', 'Trebuchet MS', 'Lucida Grande', 'Bitstream Vera Sans', 'Helvetica Neue', sans-serif;"
    , "      color: #293c4b;"
    , "    }"
    , "    a { color: #60B5CC; text-decoration: none; }"
    , "    a:hover { text-decoration: underline; }"
    , "  </style>"
    , "</head>"
    , ""
    , "<body>"
    , "  <script type=\"text/javascript\">" ++ initCode ++ "</script>"
    , "</body>"
    , ""
    , "</html>"
    ]



-- CODE


makeCodeHtml :: String -> String -> String -> H.Html
makeCodeHtml title lang code =
  do  H.head $ do
        H.title $ H.toHtml title
        H.style ! A.type_ "text/css" $ H.toHtml codeStyle
        H.style ! A.type_ "text/css" $ H.toHtml (Kate.styleToCss Kate.haddock)
      H.body $ do
        H.toHtml $ Kate.formatHtmlBlock formatOptions (Kate.highlightAs lang code)


formatOptions :: Kate.FormatOptions
formatOptions =
  Kate.FormatOptions
    { Kate.numberLines = True
    , Kate.startNumber = 1
    , Kate.lineAnchors = False
    , Kate.titleAttributes = False
    , Kate.codeClasses = []
    , Kate.containerClasses = []
    }


codeStyle :: String
codeStyle =
  unlines
    [ "@import url(http://fonts.googleapis.com/css?family=Source+Code+Pro);"
    , "html, head, body {"
    , "  margin: 0;"
    , "  height: 100%;"
    , "}"
    , "body {"
    , "  font-family: 'Source Code Pro', monospace;"
    , "}"
    ]

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.Help (makeHtml, makeCodeHtml, makeDebuggerHtml) where

import qualified Data.ByteString.Char8 as BSC
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified StaticFiles



-- DEBUGGER


makeDebuggerHtml :: String -> String -> String -> H.Html
makeDebuggerHtml title host file =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.title $ H.toHtml title
      H.script ! A.src debuggerPath $ ""
      H.style ! A.type_ "text/css" $ H.toHtml debuggerStyle

    H.body $ do
      return ()

    H.script $ H.preEscapedToMarkup $
      "Elm.Debugger.fullscreen({ flags: null, file: '" ++ file ++ "', host: '" ++ host ++ "' });"


debuggerPath :: H.AttributeValue
debuggerPath =
  H.toValue ('/' : StaticFiles.debuggerPath)


debuggerStyle :: String
debuggerStyle =
  unlines
    [ "@import url(http://fonts.googleapis.com/css?family=Source+Sans+Pro|Source+Code+Pro);"
    , "html, head, body {"
    , "  margin: 0;"
    , "  height: 100%;"
    , "}"
    ]



-- PAGES


makeHtml :: String -> String -> String -> BSC.ByteString
makeHtml title jsFile initCode =
  BSC.pack $ unlines $
    [ "<html>"
    , ""
    , "<head>"
    , "  <title>" ++ title ++ "</title>"
    , "  <link rel='icon' href='/" ++ StaticFiles.faviconPath ++ "' sizes='32x32'>"
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


makeCodeHtml :: String -> String -> H.Html
makeCodeHtml title code =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.title $ H.toHtml title
      H.style ! A.type_ "text/css" $ H.toHtml codeStyle

      H.link ! A.rel "stylesheet" ! A.href "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.3.0/styles/default.min.css"
      H.script ! A.src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.3.0/highlight.min.js" $ ""
      H.script $ "if (hljs) { hljs.initHighlightingOnLoad(); }"

    H.body $ do
      H.pre $ H.code $ H.toHtml code


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

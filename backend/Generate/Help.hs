{-# OPTIONS_GHC -Wall #-}
module Generate.Help (makeHtml) where

import qualified Data.ByteString.Char8 as BSC


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


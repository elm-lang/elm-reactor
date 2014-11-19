{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Compile (toHtml, toJson) where

import Control.Applicative ((<$>),(<*>))
import qualified Data.Text as Text
import qualified Text.Blaze as Blaze
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Utils as Utils


-- ACTUALLY COMPILE STUFF

compile :: FilePath -> IO (Either String String)
compile filePath =
  do  result <- Utils.unwrappedRun "elm-make" [filePath]
      case result of
        Left (Utils.MissingExe msg) ->
          return (Left msg)

        Left (Utils.CommandFailed out err) ->
          return (Left (out ++ err))

        Right _ ->
          do  code <- readFile "elm.js"
              return (Right code)


-- TO JSON

toJson :: FilePath -> IO String
toJson filePath =
  do  result <- compile filePath
      case result of
        Right code ->
          return (jsonReply "success" code)

        Left err ->
          return (jsonReply "error" err)


jsonReply :: String -> String -> String
jsonReply field value =
    concat [ "{ ", show field, " : ", show value, " }" ]


-- TO HTML

toHtml :: Bool -> FilePath -> IO H.Html
toHtml debug filePath =
  do  src <- readFile filePath
      result <- compile filePath
      case (,) <$> Compiler.parseDependencies src <*> result of
        Right ((name, _deps), code) ->
            return $ htmlDocument (Module.nameToString name) $
                H.script $ Blaze.preEscapedToMarkup (code ++ initialize debug name filePath)

        Left errMsg ->
            return $ htmlDocument "Oops!" $
                H.span ! A.style "font-family: monospace;" $
                    Blaze.preEscapedToMarkup (addSpaces errMsg)


htmlDocument :: String -> H.Html -> H.Html
htmlDocument title content =
  H.docTypeHtml $ do 
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.title (H.toHtml title)
      H.style $ Blaze.preEscapedToMarkup myStyle
--      H.script ! A.src (H.toValue ("/debugger.js" :: String)) $ ""
    H.body $ do
      content


myStyle :: Text.Text
myStyle =
    "html, head, body { padding:0; margin:0; }\n\
    \body { font-family: calibri, helvetica, arial, sans-serif; }\n\
    \a:link { text-decoration: none; color: rgb(15,102,230); }\n\
    \a:visited { text-decoration: none; }\n\
    \a:active { text-decoration: none; }\n\
    \a:hover { text-decoration: underline; color: rgb(234,21,122); }\n\
    \html,body { height: 100%; margin: 0px; }\n"



addSpaces :: String -> String
addSpaces str =
  case str of
    ' ' : ' ' : rest ->
        " &nbsp;" ++ addSpaces rest

    '\n' : rest ->
        "<br>" ++ addSpaces rest

    c : rest ->
        c : addSpaces rest

    [] -> []


initialize :: Bool -> Module.Name -> FilePath -> String
initialize debug name filePath =
  let moduleName = "Elm." ++ Module.nameToString name
  in
      "var runningElmModule =\n    " ++
      case debug of
        True -> "Elm.debugFullscreen(" ++ moduleName ++ ", \"" ++ filePath ++ "\");"
        False -> "Elm.fullscreen(" ++ moduleName ++ ");"

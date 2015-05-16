{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Compile (toHtml, toJson) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (when)
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
  do  result <- Utils.unwrappedRun "elm-make" [ "--yes", filePath ]
      case result of
        Left (Utils.MissingExe msg) ->
          return (Left msg)

        Left (Utils.CommandFailed out err) ->
          return (Left (out ++ err))

        Right _ ->
          do  code <- readFile "elm.js"
              return (Right code)


getName :: FilePath -> String -> Either String Module.Name
getName filePath sourceCode =
  case Compiler.parseDependencies sourceCode of
    Right (name, _deps) ->
        Right name

    Left errors ->
        Left (concatMap (Compiler.errorToString filePath sourceCode) errors)


-- TO JSON

toJson :: FilePath -> IO String
toJson filePath =
  do  sourceCode <- readFile filePath
      result <- compile filePath
      case (,) <$> getName filePath sourceCode <*> result of
        Right (name, code) ->
          return $
            "{ \"name\": " ++ show (Module.nameToString name) ++
            ", \"code\": " ++ show code ++ " }"

        Left err ->
          return $
            "{ \"error\": " ++ show ( err) ++ " }"


-- TO HTML

toHtml :: Bool -> FilePath -> IO H.Html
toHtml debug filePath =
  do  sourceCode <- readFile filePath
      result <- compile filePath
      case (,) <$> getName filePath sourceCode <*> result of
        Right (name, code) ->
            return $ htmlDocument (Module.nameToString name) $
                do  H.script $ Blaze.preEscapedToMarkup code

                    when debug $ do
                        script "/_reactor/debug.js"

                    H.script $ Blaze.preEscapedToMarkup (initialize debug name filePath)

        Left errMsg ->
            return $ htmlDocument "Oops!" $
                H.pre ! A.style "margin: 0; padding: 8px;" $
                  Blaze.toMarkup errMsg


htmlDocument :: String -> H.Html -> H.Html
htmlDocument title content =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.title (H.toHtml title)
      H.style $ Blaze.preEscapedToMarkup myStyle
    H.body $ do
      content


script :: FilePath -> H.Html
script path =
    H.script ! A.src (H.toValue path) $ ""


myStyle :: Text.Text
myStyle =
    "html, head, body { padding:0; margin:0; }\n\
    \body { font-family: calibri, helvetica, arial, sans-serif; }\n\
    \a:link { text-decoration: none; color: rgb(15,102,230); }\n\
    \a:visited { text-decoration: none; }\n\
    \a:active { text-decoration: none; }\n\
    \a:hover { text-decoration: underline; color: rgb(234,21,122); }\n\
    \html,body { height: 100%; margin: 0px; }\n"


initialize :: Bool -> Module.Name -> FilePath -> String
initialize debug name filePath =
  let moduleName = Module.nameToString name
  in
      "var runningElmModule =\n    " ++
      case debug of
        True -> "Elm.fullscreenDebug('" ++ moduleName ++ "', '" ++ filePath ++ "');"
        False -> "Elm.fullscreen(Elm." ++ moduleName ++ ");"

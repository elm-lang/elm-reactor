{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Compile (toHtml, toJson) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (removeFile)
import qualified Text.Blaze as Blaze
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import qualified Elm.Utils as Utils
import qualified Generate.Index as GI


-- ACTUALLY COMPILE STUFF


compile :: FilePath -> IO (Either Json.Value (Text.Text, String))
compile filePath =
  let
    tempJsFile =
      "it-is-safe-to-delete-this-file.js"
  in
  do  result <- Utils.unwrappedRun "elm-make" [ "--yes", filePath, "--report=json", "--output=" ++ tempJsFile ]
      case result of
        Left (Utils.MissingExe msg) ->
          return $ Left (jsonString msg)

        Left (Utils.CommandFailed out err) ->
          return $ Left (jsonValue (out ++ err) out)

        Right _ ->
          do  code <- Text.readFile tempJsFile
              removeFile tempJsFile
              source <- readFile filePath
              return $ Right $ (,) code $
                case Compiler.parseDependencies source of
                  Left _ ->
                    error "impossible"

                  Right (name, _) ->
                    Module.nameToString name



jsonString :: String -> Json.Value
jsonString string =
  Json.String (Text.pack string)


jsonValue :: String -> String -> Json.Value
jsonValue backup json =
  case Json.decode (BS.pack json) of
    Just value ->
      value

    Nothing ->
      jsonString backup



-- TO JSON


toJson :: FilePath -> IO BS.ByteString
toJson filePath =
  do  result <- compile filePath
      pkg <- GI.getPkg
      return $ Json.encode $ Json.object $
        case result of
          Right (code, moduleName) ->
            [ "pkg" .= pkg
            , "code" .= injectHooks pkg moduleName code
            ]

          Left msg ->
            [ "error" .= msg
            ]


injectHooks :: Pkg.Name -> String -> Text.Text -> Text.Text
injectHooks (Pkg.Name user project) moduleName code =
  let
    (normalStuff, ending) =
      Text.breakOnEnd "var Elm = {};" code

    escape str =
      map (\c -> if c == '-' || c == '.' then '_' else c) str

    main =
      Text.pack $
        '_' : escape user
        ++ "$" ++ escape project
        ++ "$" ++ escape moduleName
        ++ "$main"
  in
    Text.concat
      [ normalStuff
      , "this.elm_reactor_hook = {\n"
      , "\tprogram: typeof ", main, " === 'undefined' ? null : _elm_lang$core$Native_Scheduler.mainToProgram('", Text.pack moduleName, "', ", main, "),\n"
      , "\tmanagers: _elm_lang$core$Native_Scheduler.effectManagers\n"
      , "};\n"
      , "return;\n"
      , ending
      ]


-- TO HTML


toHtml :: FilePath -> IO H.Html
toHtml filePath =
  do  result <- compile filePath
      case result of
        Right (code, name) ->
            return $ htmlDocument name $
                do  H.script $ Blaze.preEscapedToMarkup code
                    H.script $ Blaze.preEscapedToMarkup $
                      "Elm." ++ name ++ ".fullscreen();"

        Left errMsg ->
            return $ htmlDocument "Oops!" $
                H.pre ! A.style "margin: 0; padding: 8px;" $
                  Blaze.unsafeLazyByteString (Json.encode errMsg)


htmlDocument :: String -> H.Html -> H.Html
htmlDocument title content =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.title (H.toHtml title)
      H.style $ Blaze.preEscapedToMarkup myStyle
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

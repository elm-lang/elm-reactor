{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Compile (toJavaScript) where

import qualified Data.Aeson as Json
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Directory (removeFile)

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Utils as Utils
import qualified StaticFiles



-- ACTUALLY COMPILE STUFF


compile :: FilePath -> IO (Either String (BS.ByteString, String))
compile filePath =
  let
    tempJsFile =
      "it-is-safe-to-delete-this-file.js"
  in
  do  result <- Utils.unwrappedRun "elm-make" [ "--yes", filePath, "--output=" ++ tempJsFile ]
      case result of
        Left (Utils.MissingExe msg) ->
          return $ Left msg

        Left (Utils.CommandFailed out err) ->
          return $ Left (out ++ err)

        Right _ ->
          do  code <- BS.readFile tempJsFile
              removeFile tempJsFile
              source <- readFile filePath
              return $ Right $ (,) code $
                case Compiler.parseDependencies source of
                  Left _ ->
                    error "impossible"

                  Right (_, name, _) ->
                    Module.nameToString name



-- TO JAVASCRIPT


toJavaScript :: FilePath -> IO BS.ByteString
toJavaScript filePath =
  do  result <- compile filePath
      case result of
        Right (code, name) ->
          return $ BS.append code $ BS.pack $
            "var runElmProgram = Elm." ++ name ++ ".fullscreen;"

        Left errMsg ->
          return $ BS.concat $
            [ StaticFiles.errors
            , BS.pack $ "function runElmProgram() {\n\tElm.Errors.fullscreen("
            , LBS.toStrict (Json.encode errMsg)
            , ");\n}"
            ]

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Maybe (isJust)
import qualified Data.ByteString.Char8 as BSC
import qualified Network.WebSockets.Snap as WSS
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Text.Highlighting.Kate as Kate

import qualified StaticFiles
import qualified Compile
import qualified Generate.Help as Generate
import qualified Generate.Index as Index
import qualified Generate.NotFound as NotFound
import qualified Socket
import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg
import Elm.Utils ((|>))


data Flags = Flags
    { address :: String
    , port :: Int
    }
    deriving (Data,Typeable,Show,Eq)


flags :: Flags
flags = Flags
  { address = "localhost"
      &= help "set the address of the server (e.g. look into 0.0.0.0 if you want to try stuff on your phone)"
      &= typ "ADDRESS"

  , port = 8000
      &= help "set the port of the reactor (default: 8000)"

  } &= help
        "Interactive development tool that makes it easy to develop and debug Elm programs.\n\
        \    Read more about it at <https://github.com/elm-lang/elm-reactor>."
    &= helpArg
        [ explicit
        , name "help"
        , name "h"
        ]
    &= versionArg
        [ explicit, name "version", name "v"
        , summary (Pkg.versionToString Compiler.version)
        ]
    &= summary startupMessage


config :: BSC.ByteString -> Int -> Config Snap a
config bindSpec portNumber =
    defaultConfig
      |> setBind bindSpec
      |> setPort portNumber
      |> setAccessLog ConfigNoLog
      |> setErrorLog ConfigNoLog


-- | Set up the reactor.
main :: IO ()
main =
  do  cargs <- cmdArgs flags
      putStrLn startupMessage
      httpServe (config (BSC.pack (address cargs)) (port cargs)) $
        serveFiles
        <|> route [ ("socket", socket) ]
        <|> serveDirectoryWith directoryConfig "."
        <|> error404


startupMessage :: String
startupMessage =
  "elm reactor " ++ Pkg.versionToString Compiler.version


directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
  let
    customGenerator directory =
      do  info <- liftIO (Index.getInfo directory)
          modifyResponse $ setContentType "text/html; charset=utf-8"
          writeBS (Index.toHtml info)
  in
    fancyDirectoryConfig
      { indexFiles = []
      , indexGenerator = customGenerator
      }


socket :: Snap ()
socket =
    maybe error400 socketSnap =<< getParam "file"
  where
    socketSnap fileParam =
         WSS.runWebSocketsSnap $ Socket.fileChangeApp $ BSC.unpack fileParam


error400 :: Snap ()
error400 =
    modifyResponse $ setResponseStatus 400 "Bad Request"


error404 :: Snap ()
error404 =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBS NotFound.html



-- SERVE FILES


serveFiles :: Snap ()
serveFiles =
  do  file <- getSafePath
      serveAssets file
        <|> do  exists <- liftIO $ doesFileExist file
                guard exists
                serveElm file <|> serveFileWithHighlight file


serveHtml :: MonadSnap m => H.Html -> m ()
serveHtml html =
  do  modifyResponse (setContentType "text/html")
      writeBuilder (Blaze.renderHtmlBuilder html)



-- SERVE FILES + CODE HIGHLIGHTING


serveFileWithHighlight :: FilePath -> Snap ()
serveFileWithHighlight file =
  let
    languages =
      Kate.languagesByFilename (takeFileName file)
  in
    case languages of
      [] ->
        serveFileWithoutDownload file

      lang : _ ->
        do  code <- liftIO (readFile file)
            serveHtml $ Generate.makeCodeHtml ('~' : '/' : file) lang code


serveFileWithoutDownload :: FilePath -> Snap ()
serveFileWithoutDownload file =
  let
    possibleExtensions =
      getSubExts (takeExtensions file)
  in
    case mconcat (map (flip HashMap.lookup defaultMimeTypes) possibleExtensions) of
      Nothing ->
        serveFileAs "text/plain" file

      Just mimeType ->
        serveFileAs mimeType file


getSubExts :: String -> [String]
getSubExts fullExtension =
  if null fullExtension then
    []

  else
    fullExtension : getSubExts (takeExtensions (drop 1 fullExtension))



-- SERVE ELM


serveElm :: FilePath -> Snap ()
serveElm file =
  pass

{-
  do  guard (takeExtension file == ".elm")

      debugParam <- getParam "debug"
      let debug = isJust debugParam

      result <- liftIO $ Compile.toHtml debug file
      serveHtml result
-}



-- SERVE STATIC ASSETS


serveAssets :: FilePath -> Snap ()
serveAssets file =
  case List.lookup file staticAssets of
    Nothing ->
      pass

    Just (content, mimeType) ->
      do  modifyResponse (setContentType $ BSC.pack (mimeType ++ ";charset=utf-8"))
          writeBS content


type MimeType =
  String


staticAssets :: [(FilePath, (BSC.ByteString, MimeType))]
staticAssets =
    [ StaticFiles.faviconPath ==>
        (StaticFiles.favicon, "image/x-icon")
    , StaticFiles.waitingPath ==>
        (StaticFiles.waiting, "image/gif")
    , StaticFiles.indexPath ==>
        (StaticFiles.index, "application/javascript")
    , StaticFiles.notFoundPath ==>
        (StaticFiles.notFound, "application/javascript")
    ]


(==>) :: a -> b -> (a,b)
(==>) = (,)

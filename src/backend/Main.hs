{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Char8 as BSC
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Network.WebSockets.Snap as WSS
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

import qualified StaticFiles as SF
import qualified Compile
import qualified Generate.Help as Generate
import qualified Generate.Index as Index
import qualified Generate.NotFound as NotFound
import qualified Socket
import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg
import Elm.Utils ((|>))



-- FLAGS


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



-- START THE REACTOR


config :: BSC.ByteString -> Int -> Config Snap a
config bindSpec portNumber =
    defaultConfig
      |> setBind bindSpec
      |> setPort portNumber
      |> setAccessLog ConfigNoLog
      |> setErrorLog ConfigNoLog


main :: IO ()
main =
  do  setLocaleEncoding utf8
      cargs <- cmdArgs flags

      putStrLn startupMessage

      httpServe (config (BSC.pack (address cargs)) (port cargs)) $
        serveFiles
        <|> route [ ("_compile", compile) ]
        <|> route [ ("_changes", socket) ]
        <|> serveDirectoryWith directoryConfig "."
        <|> serveAssets
        <|> error404



-- HELPERS


startupMessage :: String
startupMessage =
  "elm-reactor " ++ Pkg.versionToString Compiler.version


directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
  let
    customGenerator directory =
      do  info <- liftIO (Index.getInfo directory)
          modifyResponse $ setContentType "text/html; charset=utf-8"
          writeBuilder (Blaze.renderHtmlBuilder (Index.toHtml info))
  in
    fancyDirectoryConfig
      { indexFiles = []
      , indexGenerator = customGenerator
      }


compile :: Snap ()
compile =
  do  file <- getSafePath
      guard =<< liftIO (doesFileExist file)
      modifyResponse (setContentType "text/javascript")
      writeBS =<< liftIO (Compile.toJavaScript file)


socket :: Snap ()
socket =
  do  file <- getSafePath
      guard =<< liftIO (doesFileExist file)
      WSS.runWebSocketsSnap (Socket.watchFile file)


error404 :: Snap ()
error404 =
  do  modifyResponse $ setResponseStatus 404 "Not Found"
      modifyResponse $ setContentType "text/html; charset=utf-8"
      writeBuilder (Blaze.renderHtmlBuilder NotFound.html)



-- SERVE FILES


serveFiles :: Snap ()
serveFiles =
  do  file <- getSafePath
      guard =<< liftIO (doesFileExist file)
      serveElm file <|> serveFilePretty file


serveHtml :: MonadSnap m => H.Html -> m ()
serveHtml html =
  do  modifyResponse (setContentType "text/html")
      writeBuilder (Blaze.renderHtmlBuilder html)



-- SERVE FILES + CODE HIGHLIGHTING


serveFilePretty :: FilePath -> Snap ()
serveFilePretty file =
  let
    possibleExtensions =
      getSubExts (takeExtensions file)
  in
    case mconcat (map lookupMimeType possibleExtensions) of
      Nothing ->
        serveCode file

      Just mimeType ->
        serveFileAs mimeType file


getSubExts :: String -> [String]
getSubExts fullExtension =
  if null fullExtension then
    []

  else
    fullExtension : getSubExts (takeExtensions (drop 1 fullExtension))


serveCode :: String -> Snap ()
serveCode file =
  do  code <- liftIO (readFile file)
      serveHtml $ Generate.makeCodeHtml ('~' : '/' : file) code



-- SERVE ELM


serveElm :: FilePath -> Snap ()
serveElm file =
  do  guard (takeExtension file == ".elm")
      serveHtml (Generate.makeElmHtml file)



-- SERVE STATIC ASSETS


serveAssets :: Snap ()
serveAssets =
  do  file <- getSafePath
      case HashMap.lookup file staticAssets of
        Nothing ->
          pass

        Just (content, mimeType) ->
          do  modifyResponse (setContentType $ BSC.pack (mimeType ++ ";charset=utf-8"))
              writeBS content


type MimeType =
  String


staticAssets :: HashMap.HashMap FilePath (BSC.ByteString, MimeType)
staticAssets =
  HashMap.fromList
    [ SF.faviconPath  ==> (SF.favicon , "image/x-icon")
    , SF.waitingPath  ==> (SF.waiting , "image/gif")
    , SF.indexPath    ==> (SF.index   , "application/javascript")
    , SF.notFoundPath ==> (SF.notFound, "application/javascript")
    ]


(==>) :: a -> b -> (a,b)
(==>) = (,)



-- MIME TYPES


lookupMimeType :: FilePath -> Maybe BSC.ByteString
lookupMimeType ext =
  HashMap.lookup ext mimeTypeDict


mimeTypeDict :: HashMap.HashMap FilePath BSC.ByteString
mimeTypeDict =
  HashMap.fromList
    [ ".asc"     ==> "text/plain"
    , ".asf"     ==> "video/x-ms-asf"
    , ".asx"     ==> "video/x-ms-asf"
    , ".avi"     ==> "video/x-msvideo"
    , ".bz2"     ==> "application/x-bzip"
    , ".css"     ==> "text/css"
    , ".dtd"     ==> "text/xml"
    , ".dvi"     ==> "application/x-dvi"
    , ".gif"     ==> "image/gif"
    , ".gz"      ==> "application/x-gzip"
    , ".htm"     ==> "text/html"
    , ".html"    ==> "text/html"
    , ".ico"     ==> "image/x-icon"
    , ".jpeg"    ==> "image/jpeg"
    , ".jpg"     ==> "image/jpeg"
    , ".js"      ==> "text/javascript"
    , ".json"    ==> "application/json"
    , ".m3u"     ==> "audio/x-mpegurl"
    , ".mov"     ==> "video/quicktime"
    , ".mp3"     ==> "audio/mpeg"
    , ".mpeg"    ==> "video/mpeg"
    , ".mpg"     ==> "video/mpeg"
    , ".ogg"     ==> "application/ogg"
    , ".pac"     ==> "application/x-ns-proxy-autoconfig"
    , ".pdf"     ==> "application/pdf"
    , ".png"     ==> "image/png"
    , ".qt"      ==> "video/quicktime"
    , ".sig"     ==> "application/pgp-signature"
    , ".spl"     ==> "application/futuresplash"
    , ".svg"     ==> "image/svg+xml"
    , ".swf"     ==> "application/x-shockwave-flash"
    , ".tar"     ==> "application/x-tar"
    , ".tar.bz2" ==> "application/x-bzip-compressed-tar"
    , ".tar.gz"  ==> "application/x-tgz"
    , ".tbz"     ==> "application/x-bzip-compressed-tar"
    , ".text"    ==> "text/plain"
    , ".tgz"     ==> "application/x-tgz"
    , ".ttf"     ==> "application/x-font-truetype"
    , ".txt"     ==> "text/plain"
    , ".wav"     ==> "audio/x-wav"
    , ".wax"     ==> "audio/x-ms-wax"
    , ".wma"     ==> "audio/x-ms-wma"
    , ".wmv"     ==> "video/x-ms-wmv"
    , ".xbm"     ==> "image/x-xbitmap"
    , ".xml"     ==> "text/xml"
    , ".xpm"     ==> "image/x-xpixmap"
    , ".xwd"     ==> "image/x-xwindowdump"
    , ".zip"     ==> "application/zip"
    ]


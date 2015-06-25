{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>),(<|>))
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Maybe (isJust)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Version as Version
import qualified Network.WebSockets.Snap as WSS
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

import Index
import qualified Compile
import qualified Socket
import qualified Utils
import Paths_elm_reactor (version)
import qualified Elm.Compiler as Compiler
import Elm.Utils ((|>))


data Flags = Flags
    { address :: String
    , port :: Int
    }
    deriving (Data,Typeable,Show,Eq)


flags :: Flags
flags = Flags
  { address = "0.0.0.0"
      &= help "set the address of the server (0.0.0.0, localhost, 127.0.0.1)"
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
        , summary (Version.showVersion version)
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
          serveElm
          <|> route [ ("socket", socket) ]
          <|> serveDirectoryWith directoryConfig "."
          <|> serveAssets
          <|> error404


startupMessage :: String
startupMessage =
  "Elm Reactor " ++ Version.showVersion version
  ++ " (Elm Platform " ++ Compiler.version ++ ")"


directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
    fancyDirectoryConfig
    { indexFiles = []
    , indexGenerator = elmIndexGenerator
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
    modifyResponse $ setResponseStatus 404 "Not Found"


-- SERVE ELM CODE

serveElm :: Snap ()
serveElm =
  let despace = map (\c -> if c == '+' then ' ' else c) in
  do  file <- despace . BSC.unpack . rqPathInfo <$> getRequest
      debugParam <- getParam "debug"
      let debug = isJust debugParam
      exists <- liftIO $ doesFileExist file
      guard (exists && takeExtension file == ".elm")
      result <- liftIO $ Compile.toHtml debug file
      serveHtml result


serveHtml :: MonadSnap m => H.Html -> m ()
serveHtml html =
  do  modifyResponse (setContentType "text/html")
      writeBuilder (Blaze.renderHtmlBuilder html)


-- SERVE STATIC ASSETS

serveAssets :: Snap ()
serveAssets =
  do  file <- BSC.unpack . rqPathInfo <$> getRequest
      case file `elem` staticAssets of
        True ->
          serveFile =<< liftIO (Utils.getDataFile file)

        False ->
          pass


staticAssets :: [FilePath]
staticAssets =
    [ "favicon.ico"
    , "_reactor/debug.js"
    , "_reactor/wrench.png"
    ]


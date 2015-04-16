{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
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

import Network.HTTP.Client (withManager, defaultManagerSettings, httpLbs,
  responseStatus, responseHeaders, parseUrl, RequestBody(RequestBodyLBS),
  responseBody)
import Network.HTTP.Types(Status(statusCode))
import qualified Network.HTTP.Client as HTTP (Request(path, method,
  requestHeaders, requestBody))


data Flags = Flags
    { bind :: String
    , port :: Int
    , forward :: Maybe String
    }
    deriving (Data,Typeable,Show,Eq)


flags :: Flags
flags = Flags
  { bind = "0.0.0.0" &= help "set the host to bind to (default: 0.0.0.0)" &= typ "SPEC"
  , port = 8000 &= help "set the port of the reactor (default: 8000)"
  , forward = Nothing &= help (
      "Forward requests that would otherwise return a 404 to the given\
      \ server. The path portion of the url is ignored and replaced with\
      \ the path of the request. This is useful for avoiding cross-origin web\
      \ requests when using elm-reactor to debug elm programs that make server\
      \ requests."
    ) &= typ "URL"
  } &= help "Interactive development tool that makes it easy to develop and debug Elm programs.\n\
            \    Read more about it at <https://github.com/elm-lang/elm-reactor>."
    &= helpArg [explicit, name "help", name "h"]
    &= versionArg [ explicit, name "version", name "v"
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
      httpServe (config (BSC.pack (bind cargs)) (port cargs)) $
          serveElm
          <|> route [ ("socket", socket) ]
          <|> serveDirectoryWith directoryConfig "."
          <|> serveAssets
          <|> forwardRequest (forward cargs)
          <|> error404


forwardRequest :: Maybe String -> Snap ()
forwardRequest Nothing = pass
forwardRequest (Just forwardUrl) = do
  rqHeaders <- getsRequest headers
  method_ <- getsRequest rqMethod
  uri <- getsRequest rqURI
  rqBody <- readRequestBody maxBound
  request <- makeRequest uri rqHeaders method_ rqBody
  response <- liftIO $ withManager defaultManagerSettings (httpLbs request)
  modifyResponse (
      setHeaders (responseHeaders response)
      . setResponseCode (statusCode (responseStatus response))
    )
  writeLBS (responseBody response)
  where
    setHeaders hrds response =
      foldr (uncurry setHeader) response (filter notSpecial hrds)

    notSpecial ("content-length", _) = False
    notSpecial _ = True

    makeRequest uri hrds m rqBody = liftIO $ do
      req <- parseUrl forwardUrl
      return req {
        HTTP.path = uri,
        HTTP.method = showMethod m,
        HTTP.requestHeaders = listHeaders hrds,
        HTTP.requestBody = RequestBodyLBS rqBody
      }

    showMethod (Method m_) = m_
    showMethod m_ = BSC.pack (show m_)


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
    , "_reactor/debugger/pause-button-up.png"
    , "_reactor/debugger/pause-button-down.png"
    , "_reactor/debugger/pause-button-hover.png"
    , "_reactor/debugger/play-button-up.png"
    , "_reactor/debugger/play-button-down.png"
    , "_reactor/debugger/play-button-hover.png"
    , "_reactor/debugger/restart-button-up.png"
    , "_reactor/debugger/restart-button-down.png"
    , "_reactor/debugger/restart-button-hover.png"
    ]


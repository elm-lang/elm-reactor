{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where

import Control.Applicative ((<$>),(<|>))
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Version as Version
import qualified Network.WebSockets.Snap as WSS
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.Process
import System.IO (hGetContents)
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
    { port :: Int
    , runtime :: Maybe FilePath
    }
    deriving (Data,Typeable,Show,Eq)


flags :: Flags
flags = Flags
  { port = 8000 &= help "set the port of the reactor"
  , runtime = Nothing &= typFile
              &= help "Specify a custom location for Elm's runtime system."
  } &= help "Interactive development tool that makes it easy to develop and debug Elm programs.\n\
            \    Read more about it at <https://github.com/elm-lang/elm-reactor>."
    &= helpArg [explicit, name "help", name "h"]
    &= versionArg [ explicit, name "version", name "v"
                  , summary (Version.showVersion version)
                  ]
    &= summary ("Elm Reactor " ++ Version.showVersion version ++
                ", (c) Evan Czaplicki 2011-2014")


config :: Int -> Config Snap a
config portNumber =
    defaultConfig
      |> setPort portNumber
      |> setAccessLog ConfigNoLog
      |> setErrorLog ConfigNoLog


-- | Set up the reactor.
main :: IO ()
main =
  do  cargs <- cmdArgs flags
      (_,Just h,_,_) <- createProcess $ (shell "elm --version") { std_out = CreatePipe }
      elmVer <- hGetContents h
      putStrLn (startupMessage elmVer)
      httpServe (config (port cargs)) $
          serveElm
          <|> route [ ("socket", socket) ]
          <|> serveDirectoryWith directoryConfig "."
          <|> serveAssets
          <|> error404


startupMessage :: String -> String
startupMessage elmVer =
  "Elm Reactor " ++ Version.showVersion version ++ ", backed by version " ++
  filter (/= '\n') elmVer ++ " of the compiler."


directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
    fancyDirectoryConfig
    { indexFiles = []
    , indexGenerator = elmIndexGenerator
    }


socket :: Snap ()
socket = maybe error400 socketSnap =<< getParam "file"
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
      let debug = maybe False (const True) debugParam
      exists <- liftIO $ doesFileExist file
      guard (exists && takeExtension file == ".elm")
      result <- liftIO $ Compile.toHtml debug file
      serveHtml result


serveHtml :: MonadSnap m => H.Html -> m ()
serveHtml html =
  do  _ <- setContentType "text/html" <$> getResponse
      writeBuilder (Blaze.renderHtmlBuilder html)


-- SERVE STATIC ASSETS

serveAssets :: Snap ()
serveAssets =
  do  file <- BSC.unpack . rqPathInfo <$> getRequest
      case () of
        _ | file == "debugging-runtime.js" ->
              serveFile =<< liftIO Compiler.runtimeDebugPath

          | file `elem` staticAssets ->
              serveFile =<< liftIO (Utils.getDataFile file)

          | otherwise -> pass


staticAssets :: [FilePath]
staticAssets =
    [ "favicon.ico"
    , "debugger.js"
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


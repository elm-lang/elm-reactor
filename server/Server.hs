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
import qualified Text.Blaze.Html.Renderer.Utf8 as BlazeBS
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.Process
import System.IO (hGetContents)
import Paths_elm_reactor (version)
import qualified Elm.Internal.Paths as Elm
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

import Index
import qualified Generate
import qualified Socket
import qualified Utils

data Flags = Flags
  { port :: Int
  , runtime :: Maybe FilePath
  } deriving (Data,Typeable,Show,Eq)

flags :: Flags
flags = Flags
  { port = 8000 &= help "set the port of the server"
  , runtime = Nothing &= typFile
              &= help "Specify a custom location for Elm's runtime system."
  } &= help "Quickly reload Elm projects in your browser. Just refresh to recompile.\n\
            \It serves static files and freshly recompiled Elm files."
    &= helpArg [explicit, name "help", name "h"]
    &= versionArg [ explicit, name "version", name "v"
                  , summary (Version.showVersion version)
                  ]
    &= summary ("Elm Server " ++ Version.showVersion version ++
                ", (c) Evan Czaplicki 2011-2014")


config :: Config Snap a
config = setAccessLog ConfigNoLog (setErrorLog ConfigNoLog defaultConfig)

-- | Set up the server.
main :: IO ()
main = do
  cargs <- cmdArgs flags
  (_,Just h,_,_) <- createProcess $ (shell "elm --version") { std_out = CreatePipe }
  elmVer <- hGetContents h
  putStr $ "Elm Server " ++ Version.showVersion version ++ " serving Elm " ++ elmVer
  putStrLn "Just refresh a page to recompile it!"
  httpServe (setPort (port cargs) config) $
      serveRuntime (maybe Elm.runtime id (runtime cargs))
      <|> serveElm
      <|> route [ ("socket", socket)
                ]
      <|> serveDirectoryWith directoryConfig "."
      <|> serveAssets
      <|> error404

directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig = fancyDirectoryConfig {indexGenerator = elmIndexGenerator}

runtimeName :: String
runtimeName = "elm-runtime.js"

serveRuntime :: FilePath -> Snap ()
serveRuntime runtimePath =
  do file <- BSC.unpack . rqPathInfo <$> getRequest
     guard (file == runtimeName)
     serveFileAs "application/javascript" runtimePath

socket :: Snap ()
socket = maybe error400 socketSnap =<< getParam "file"
  where
    socketSnap fileParam =
         WSS.runWebSocketsSnap $ Socket.fileChangeApp $ BSC.unpack fileParam

withFile :: (FilePath -> H.Html) -> Snap ()
withFile handler =
  do filePath <- BSC.unpack . rqPathInfo <$> getRequest
     exists <- liftIO (doesFileExist filePath)
     if not exists then error404 else
         serveHtml $ handler filePath

error400 :: Snap ()
error400 = modifyResponse $ setResponseStatus 400 "Bad Request"

error404 :: Snap ()
error404 = modifyResponse $ setResponseStatus 404 "Not Found"

serveHtml :: MonadSnap m => H.Html -> m ()
serveHtml html =
  do _ <- setContentType "text/html" <$> getResponse
     writeLBS (BlazeBS.renderHtml html)

serveElm :: Snap ()
serveElm =
  do file <- BSC.unpack . rqPathInfo <$> getRequest
     debugParam <- getParam "debug"
     let doDebug = maybe False (const True) debugParam
     exists <- liftIO $ doesFileExist file
     guard (exists && takeExtension file == ".elm")
     result <- liftIO $ Generate.html file doDebug
     serveHtml result

serveAsset :: FilePath -> Snap ()
serveAsset assetPath =
  do dataPath <- liftIO $ Utils.getDataFile assetPath
     serveFile dataPath

staticAssets :: [FilePath]
staticAssets = [ "favicon.ico"
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

serveAssets :: Snap ()
serveAssets =
  do file <- BSC.unpack . rqPathInfo <$> getRequest
     guard (file `elem` staticAssets)
     serveAsset file

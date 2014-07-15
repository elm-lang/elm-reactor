{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where

import Control.Applicative ((<$>),(<|>))
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Version as Version
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Utf8 as BlazeBS
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import System.Console.CmdArgs
import System.Directory
import System.Exit
import System.FilePath
import qualified System.FSNotify as Notify
import qualified System.FSNotify.Devel as NDevel
import System.Process
import System.IO (hGetContents, Handle)

import Paths_elm_server (version)
import qualified Elm.Internal.Paths as Elm
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

import Index
import qualified Editor
import qualified Generate

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
      <|> route [ ("debug", debug)
                , ("socket", socket)
                ]
      <|> serveDirectoryWith simpleDirectoryConfig "resources"
      <|> serveDirectoryWith simpleDirectoryConfig "build"
      <|> serveDirectoryWith directoryConfig "."
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
socket = WS.runWebSocketsSnap fileChangeApp

fileChangeApp :: WS.ServerApp
fileChangeApp pendingConnection = do
      connection <- WS.acceptRequest pendingConnection
      _ <- forkIO $ keepAlive connection
      notifyManager <- liftIO $ Notify.startManager
      updateOnChange notifyManager connection
      Notify.stopManager notifyManager

keepAlive :: WS.Connection -> IO ()
keepAlive connection =
  do WS.sendPing connection $ BSC.pack "ping"
     threadDelay $ 10 * (1000000) -- 10 seconds
     keepAlive connection

updateOnChange :: Notify.WatchManager -> WS.Connection -> IO ()
updateOnChange manager connection =
  do _ <- NDevel.treeExtExists manager "." "elm" (sendHotSwap connection)
     threadDelay maxBound

sendHotSwap :: WS.Connection -> FP.FilePath -> IO ()
sendHotSwap connection filePath =
  do (_, stdout, stderr, phandle) <- liftIO $ compileJS file
     exitCode <- waitForProcess phandle
     case (exitCode, stdout, stderr) of
       (ExitFailure _, Just out, Just err) ->
           do output <- hGetContents out 
              errorMessage <- hGetContents err
              putStrLn $ output ++ errorMessage
              return ()
       (ExitFailure _, _, _) ->
           do putStrLn "Check the console for the error"
              return ()
       (ExitSuccess, _ , _) ->
           do result <- readFile ("build" </> file `replaceExtension` "js") 
              WS.sendTextData connection $ BSC.pack result
  where
    file = FP.encodeString $ FP.filename filePath
    compileJS elmFile =
      let elmArgs = [ "--make", "--only-js", "--set-runtime=/" ++ runtimeName, elmFile ]
      in  createProcess $ (proc "elm" elmArgs) { std_out = CreatePipe }


serveElm :: Snap ()
serveElm =
  do file <- BSC.unpack . rqPathInfo <$> getRequest
     exists <- liftIO $ doesFileExist file
     guard (exists && takeExtension file == ".elm")
     fileContents <- liftIO $ readFile file
     result <- liftIO $ Generate.html "Compiled Elm" fileContents
     serveHtml result

failure :: String -> Snap ()
failure msg =
  do modifyResponse $ setResponseStatus 404 "Not found"
     writeBS $ BSC.pack msg

onSuccess :: IO (t, Maybe Handle, t1, ProcessHandle) -> Snap () -> Snap ()
onSuccess action success =
  do (_, stdout, _, handle) <- liftIO action
     exitCode <- liftIO $ waitForProcess handle
     case (exitCode, stdout) of
       (ExitFailure 127, _) ->
           failure "Error: elm compiler not found in your path."

       (ExitFailure _, Just out) ->
           failure =<< liftIO (hGetContents out)

       (ExitFailure _, Nothing) ->
           failure "See command line for error message."

       (ExitSuccess, _) -> success

debug :: Snap()
debug = withFile Editor.ide 

withFile :: (FilePath -> H.Html) -> Snap ()
withFile handler = do
  filePath <- BSC.unpack . rqPathInfo <$> getRequest
  let file = filePath
  exists <- liftIO (doesFileExist file)
  if not exists then error404 else
      serveHtml $ handler filePath

error404 :: Snap ()
error404 =
    do modifyResponse $ setResponseStatus 404 "Not found"
       serveFileAs "text/html; charset=UTF-8" "build/public/Error404.elm"

serveHtml :: MonadSnap m => H.Html -> m ()
serveHtml html =
    do _ <- setContentType "text/html" <$> getResponse
       writeLBS (BlazeBS.renderHtml html)


{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where

import Control.Applicative ((<$>),(<|>))
import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad (guard, forever, unless)
import Control.Monad.Trans (MonadIO(liftIO))
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
      ifTop (serveDirectoryWith directoryConfig ".")
      <|> serveRuntime (maybe Elm.runtime id (runtime cargs))
      <|> serveElm
      <|> route [ ("debug", edit)
                , ("compile", compile)
                , ("hotswap", hotswap)
                , ("socket", socket)
                ]
      <|> serveDirectoryWith simpleDirectoryConfig "resources"
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
socket = WS.runWebSocketsSnap pollingApp

pollingApp :: WS.ServerApp
pollingApp pendingConnection = do
      conn <- WS.acceptRequest pendingConnection
      _ <- putStrLn "Sent Hello"
      WS.sendTextData conn $ BSC.pack "Hello world!"

hotswap :: Snap ()
hotswap = maybe error404 serve =<< getParam "input"
    where
      serve src = do
        _ <- setContentType "application/javascript" <$> getResponse
        result <- liftIO . Generate.js $ BSC.unpack src
        writeBS (BSC.pack result)

compile :: Snap ()
compile = maybe error404 serve =<< getParam "input"
    where
      serve src = do
        result <- liftIO . Generate.html "Compiled Elm" $ BSC.unpack src
        serveHtml result

serveElm :: Snap ()
serveElm =
  do file <- BSC.unpack . rqPathInfo <$> getRequest
     exists <- liftIO $ doesFileExist file
     guard (exists && takeExtension file == ".elm")
     onSuccess (compile file) (serve file)
  where
    compile file =
        let elmArgs = [ "--make", "--set-runtime=/" ++ runtimeName, file ]
        in  createProcess $ (proc "elm" elmArgs) { std_out = CreatePipe }

    serve file =
        serveFileAs "text/html; charset=UTF-8" ("build" </> replaceExtension file "html")

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

edit :: Snap()
edit = withFile Editor.ide 

withFile :: (FilePath -> String -> H.Html) -> Snap ()
withFile handler = do
  filePath <- BSC.unpack . rqPathInfo <$> getRequest
  let file = filePath
  exists <- liftIO (doesFileExist file)
  if not exists then error404 else
      do content <- liftIO $ readFile file
         serveHtml $ handler filePath content

error404 :: Snap ()
error404 =
    do modifyResponse $ setResponseStatus 404 "Not found"
       serveFileAs "text/html; charset=UTF-8" "build/public/Error404.elm"

serveHtml :: MonadSnap m => H.Html -> m ()
serveHtml html =
    do _ <- setContentType "text/html" <$> getResponse
       writeLBS (BlazeBS.renderHtml html)

{--
pageTitle :: String -> String
pageTitle = dropExtension . takeBaseName
--}

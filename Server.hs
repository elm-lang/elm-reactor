{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where

import Control.Applicative ((<$>),(<|>))
import Control.Monad (guard)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Version as Version
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
      <|> serveDirectoryWith directoryConfig "."

directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig = fancyDirectoryConfig {indexGenerator = elmIndexGenerator}

runtimeName :: String
runtimeName = "elm-runtime.js"

serveRuntime :: FilePath -> Snap ()
serveRuntime runtimePath =
  do file <- BSC.unpack . rqPathInfo <$> getRequest
     guard (file == runtimeName)
     serveFileAs "application/javascript" runtimePath

serveElm :: Snap ()
serveElm =
  do file <- BSC.unpack . rqPathInfo <$> getRequest
     exists <- liftIO $ doesFileExist file
     guard (exists && takeExtension file == ".elm")
     onSuccess (compile file) (serve file)
  where
    compile file =
        let elmArgs = [ "--make", "--set-runtime=" ++ runtimeName, file ]
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

{--
pageTitle :: String -> String
pageTitle = dropExtension . takeBaseName
--}

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate (html, js) where

import Control.Monad    (forM_, when)
import Data.Maybe       (fromMaybe)
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Text.Blaze       (preEscapedToMarkup)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Elm.Internal.Utils as Elm

-- | Using a page title and the full source of an Elm program, compile down to
--   a valid HTML document.
html :: FilePath -> IO H.Html
html filePath =
  do src <- readFile filePath
     compilerResult <- compile filePath
     return . buildPage $ formatResult src compilerResult
  where
    script = H.script ! A.type_ "text/javascript"

    formatResult src compilerResult =
        case compilerResult of
          Right jsSrc ->
              do script $ preEscapedToMarkup jsSrc
                 script $ preEscapedToMarkup $ runFullscreen src
          Left err ->
              H.span ! A.style "font-family: monospace;" $
               forM_ (lines err) $ \line ->
                   do preEscapedToMarkup (addSpaces line)
                      H.br

    runFullscreen src =
        let moduleName = "Elm." ++ fromMaybe "Main" (Elm.moduleName src)
        in  "var runningElmModule = Elm.fullscreen(Elm.debuggerAttach(" ++  moduleName ++ "))"

    buildPage content = H.docTypeHtml $ do
        H.head $ do
          H.meta ! A.charset "UTF-8"
          H.title . H.toHtml $ takeFileName filePath
          H.style ! A.type_ "text/css" $ preEscapedToMarkup
              ("a:link {text-decoration: none; color: rgb(15,102,230);}\n\
               \a:visited {text-decoration: none}\n\
               \a:active {text-decoration: none}\n\
               \a:hover {text-decoration: underline; color: rgb(234,21,122);}" :: String)
        H.body $ do
          script ! A.src (H.toValue ("/elm-runtime.js" :: String)) $ ""
          content

js :: FilePath -> IO String
js filePath =
  do output <- compile filePath
     return (either (wrap "error") (wrap "success") output)
  where
    wrap :: String -> String -> String
    wrap typ msg = "{ " ++ show typ ++ " : " ++ show msg ++ " }"

addSpaces :: String -> String
addSpaces str =
  case str of
    ' ' : ' ' : rest -> " &nbsp;" ++ addSpaces rest
    c : rest -> c : addSpaces rest
    [] -> []

compile :: FilePath -> IO (Either String String)
compile filePath =
  do (exitCode, stdout, stderr) <- readProcessWithExitCode "elm" (args filePath) ""
     case exitCode of
       ExitFailure _ ->
         do removeEverything filePath
            return (Left (stdout ++ stderr))
       ExitSuccess ->
         do result <- readFile ("build" </> filePath `replaceExtension` "js")
            length result `seq` removeEverything filePath
            return (Right result)
  where
    args file =
        [ "--make"
        , "--only-js"
        , file
        ]
    removeEverything :: FilePath -> IO ()
    removeEverything file =
        do remove "cache" "elmi"
           remove "cache" "elmo"
           remove "build" "js"
        where
          remove :: String -> String -> IO ()
          remove dir ext = do
            let path = dir </> file `replaceExtension` ext
            exists <- doesFileExist path
            when exists (removeFile path)

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate (html, js) where

import Control.Monad    (forM_, when)
import Data.Maybe       (fromMaybe)
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import System.IO        (hGetContents)
import Text.Blaze       (preEscapedToMarkup)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Elm.Internal.Utils as Elm

-- | Using a page title and the full source of an Elm program, compile down to
--   a valid HTML document.
html :: FilePath -> Bool -> IO H.Html
html filePath doDebug =
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

    attachDebugger moduleName =
      case doDebug of
        True -> "Elm.debugFullscreen(" ++ moduleName ++ ", \"" ++ filePath ++ "\");"
        False -> "Elm.fullscreen(" ++ moduleName ++ ");"

    runFullscreen src =
        let moduleName = "Elm." ++ fromMaybe "Main" (Elm.moduleName src)
        in  "var runningElmModule = " ++ (attachDebugger moduleName)

    insertDebuggerScript =
      case doDebug of
        True -> script ! A.src (H.toValue ("/debugger.js" :: String)) $ ""
        False -> return ()

    buildPage content = H.docTypeHtml $ do
        H.head $ do
          H.meta ! A.charset "UTF-8"
          H.title . H.toHtml $ takeFileName filePath
          H.style ! A.type_ "text/css" $ preEscapedToMarkup
              ("a:link {text-decoration: none; color: rgb(15,102,230);}\n\
               \a:visited {text-decoration: none}\n\
               \a:active {text-decoration: none}\n\
               \a:hover {text-decoration: underline; color: rgb(234,21,122);}\n\
               \html,body {height: 100%; margin: 0px;}" :: String)
        H.body $ do
          script ! A.src (H.toValue ("/elm-runtime.js" :: String)) $ ""
          insertDebuggerScript
          content

-- | Creates the javascript for the elm program and returns it as a 
--   JSONified string with either success:<code> or error:<message>
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
  do (_, Just hout, Just herr, p) <- createProcess (proc "elm" $ args fileName)
                                     { cwd = Just directory
                                     , std_out = CreatePipe
                                     , std_err = CreatePipe
                                     }
     exitCode <- waitForProcess p
     stdout <- exitCode `seq` hGetContents hout
     stderr <- exitCode `seq` hGetContents herr
     case exitCode of
       ExitFailure _ ->
         do removeEverything directory fileName
            return (Left (stdout ++ stderr))
       ExitSuccess ->
         do result <- readFile (directory </> "build" </> fileName `replaceExtension` "js")
            length result `seq` (removeEverything directory fileName)
            return (Right result)
  where
    (directory, fileName) = splitFileName filePath
    args file =
        [ "--make"
        , "--only-js"
        , file
        ]
    removeEverything :: FilePath -> FilePath -> IO ()
    removeEverything dir file =
        do remove "cache" "elmi"
           remove "cache" "elmo"
           --remove "build" "js"
        where
          remove :: String -> String -> IO ()
          remove subdir ext = do
            let path = dir </> subdir </> file`replaceExtension` ext
            exists <- doesFileExist path
            when exists (removeFile path)

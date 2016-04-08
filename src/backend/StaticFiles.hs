{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module StaticFiles
    ( index, indexPath
    , notFound, notFoundPath
    , debugger, debuggerPath
    , favicon, faviconPath
    , waiting, waitingPath
    )
    where

import qualified Data.ByteString as BS
import Data.FileEmbed (bsToExp)
import Language.Haskell.TH (runIO)
import System.FilePath ((</>))

import qualified StaticFiles.Build as Build



-- PATHS


faviconPath :: FilePath
faviconPath =
  "_reactor/favicon.ico"


waitingPath :: FilePath
waitingPath =
  "_reactor/waiting.gif"


indexPath :: FilePath
indexPath =
  "_reactor/index.js"


notFoundPath :: FilePath
notFoundPath =
  "_reactor/notFound.js"


debuggerPath :: FilePath
debuggerPath =
  "_reactor/debugger.js"



-- RAW RESOURCES


index :: BS.ByteString
index =
  $(bsToExp =<< runIO (Build.compile ("src" </> "pages" </> "Index.elm")))


notFound :: BS.ByteString
notFound =
  $(bsToExp =<< runIO (Build.compile ("src" </> "pages" </> "NotFound.elm")))


debugger :: BS.ByteString
debugger =
  $(bsToExp =<< runIO (
    BS.append
      <$> Build.compile ("src" </> "debugger" </> "Debugger.elm")
      <*> Build.debuggerFooter
  ))


favicon :: BS.ByteString
favicon =
  $(bsToExp =<< runIO Build.favicon)


waiting :: BS.ByteString
waiting =
  $(bsToExp =<< runIO Build.waiting)


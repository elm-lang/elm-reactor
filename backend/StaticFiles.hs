{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module StaticFiles
    ( debuggerAgent, debuggerAgentPath
    , debuggerInterfaceJs, debuggerInterfaceJsPath
    , debuggerInterfaceHtml, debuggerInterfaceHtmlPath
    , index, indexPath
    , notFound, notFoundPath, favicon, faviconPath
    )
    where

import qualified Data.ByteString as BS
import Data.FileEmbed (bsToExp)
import Language.Haskell.TH (runIO)

import qualified StaticFiles.Build as Build


debuggerAgent :: BS.ByteString
debuggerAgent =
  $(bsToExp =<< runIO Build.debuggerAgent)


debuggerInterfaceJs :: BS.ByteString
debuggerInterfaceJs =
  $(bsToExp =<< runIO Build.debuggerInterfaceJs)


debuggerInterfaceHtml :: BS.ByteString
debuggerInterfaceHtml =
  $(bsToExp =<< runIO Build.debuggerInterfaceHtml)


index :: BS.ByteString
index =
  $(bsToExp =<< runIO (Build.navigationPage "Index.elm"))


notFound :: BS.ByteString
notFound =
  $(bsToExp =<< runIO (Build.navigationPage "NotFound.elm"))


faviconPath :: FilePath
faviconPath =
  "_reactor/favicon.ico"


favicon :: BS.ByteString
favicon =
  $(bsToExp =<< runIO Build.favicon)


debuggerAgentPath :: FilePath
debuggerAgentPath =
  "_reactor/debug-agent.js"


debuggerInterfaceJsPath :: FilePath
debuggerInterfaceJsPath =
  "_reactor/debug-interface.js"


debuggerInterfaceHtmlPath :: FilePath
debuggerInterfaceHtmlPath =
  "_reactor/debug-interface.html"


indexPath :: FilePath
indexPath =
  "_reactor/index.js"


notFoundPath :: FilePath
notFoundPath =
  "_reactor/notFound.js"

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module StaticFiles
    ( debugger, debuggerPath
    , index, indexPath
    , notFound, notFoundPath
    )
    where

import qualified Data.ByteString as BS
import Data.FileEmbed (bsToExp)
import Language.Haskell.TH (runIO)

import qualified StaticFiles.Build as Build


debugger :: BS.ByteString
debugger =
  $(bsToExp =<< runIO Build.debugger)


index :: BS.ByteString
index =
  $(bsToExp =<< runIO (Build.navigationPage "Index.elm"))


notFound :: BS.ByteString
notFound =
  $(bsToExp =<< runIO (Build.navigationPage "NotFound.elm"))


debuggerPath :: FilePath
debuggerPath =
  "_reactor/debug.js"


indexPath :: FilePath
indexPath =
  "_reactor/index.js"


notFoundPath :: FilePath
notFoundPath =
  "_reactor/notFound.js"

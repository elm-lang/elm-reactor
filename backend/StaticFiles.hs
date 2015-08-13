{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module StaticFiles
    ( debugger, index
    , debuggerPath, indexPath
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
  $(bsToExp =<< runIO Build.index)


debuggerPath :: FilePath
debuggerPath =
  "_reactor/debug.js"


indexPath :: FilePath
indexPath =
  "_reactor/index.js"

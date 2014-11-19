{-# OPTIONS_GHC -Wall #-}
module Utils where

import qualified Elm.Utils as Utils
import qualified Paths_elm_reactor as Reactor


{-| The absolute path to a data file -}
getDataFile :: FilePath -> IO FilePath
getDataFile name =
    Utils.getAsset "reactor" Reactor.getDataFileName name

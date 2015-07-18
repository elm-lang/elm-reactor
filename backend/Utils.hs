{-# OPTIONS_GHC -Wall #-}
module Utils where

import qualified Elm.Utils as Utils
import qualified Paths_elm_reactor as Reactor


{-| The absolute path to a data file -}
getDataFile :: FilePath -> IO FilePath
getDataFile name =
    Utils.getAsset "reactor" Reactor.getDataFileName name


splitList :: Eq a => a -> [a] -> [[a]]
splitList element = foldr handle []
  where
    handle a [] = handle a [[]]
    handle a (x:xs) = if a == element then []:x:xs else (a:x):xs

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.Index (getInfo, toHtml) where

import Control.Monad
import Control.Monad.Except (liftIO, runExceptT, throwError)
import Data.Aeson as Json
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as N
import qualified Elm.Package.Paths as Paths
import qualified Elm.Package.Solution as S
import qualified Elm.Package.Version as V
import System.Directory ( doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ( (</>), splitDirectories )

import qualified Generate.Help as Help
import qualified StaticFiles


-- INFO

data Info = Info
    { _pwd :: [String]
    , _dirs :: [FilePath]
    , _files :: [FilePath]
    , _pkg :: Maybe PackageInfo
    , _readme :: Maybe String
    }


data PackageInfo = PackageInfo
    { _version :: V.Version
    , _repository :: String
    , _summary :: String
    , _dependencies :: [(N.Name, V.Version)]
    }


-- TO JSON

instance ToJSON Info where
  toJSON info =
    object
      [ "pwd" .= _pwd info
      , "dirs" .= _dirs info
      , "files" .= _files info
      , "pkg" .= _pkg info
      , "readme" .= _readme info
      ]


instance ToJSON PackageInfo where
  toJSON pkgInfo =
    object
      [ "version" .= _version pkgInfo
      , "repository" .= _repository pkgInfo
      , "summary" .= _summary pkgInfo
      , "dependencies" .= _dependencies pkgInfo
      ]


-- GENERATE HTML

toHtml :: Info -> BSC.ByteString
toHtml info@(Info pwd _ _ _ _) =
  Help.makeHtml
    (List.intercalate "/" ("~" : pwd))
    ("/" ++ StaticFiles.indexPath)
    ("Elm.fullscreen(Elm.Index, { info: " ++ LBSC.unpack (Json.encode info) ++ " });")


-- GET INFO FOR THIS LOCATION

getInfo :: FilePath -> IO Info
getInfo directory =
  do  packageInfo <- getPackageInfo
      (dirs, files) <- getDirectoryInfo directory
      readme <- getReadme directory
      return $ Info
          { _pwd = toPwd directory
          , _dirs = dirs
          , _files = files
          , _pkg = packageInfo
          , _readme = readme
          }


toPwd :: FilePath -> [String]
toPwd directory =
  case splitDirectories directory of
    "." : path ->
        path

    path ->
        path


getPackageInfo :: IO (Maybe PackageInfo)
getPackageInfo =
  fmap (either (const Nothing) Just) $ runExceptT $
    do  exists <- liftIO (doesFileExist Paths.description)
        when (not exists) (throwError "file not found")
        desc <- Desc.read Paths.description
        solution <- S.read Paths.solvedDependencies
        let publicSolution =
              Map.intersection solution (Map.fromList (Desc.dependencies desc))
        return $ PackageInfo
          { _version = Desc.version desc
          , _repository = Desc.repo desc
          , _summary = Desc.summary desc
          , _dependencies = Map.toList publicSolution
          }


getDirectoryInfo :: FilePath -> IO ([FilePath], [FilePath])
getDirectoryInfo directory =
  do  directoryContents <-
          getDirectoryContents directory

      allDirectories <-
          filterM (doesDirectoryExist . (directory </>)) directoryContents

      let isLegit name =
            List.notElem name [".", "..", "elm-stuff"]

      let directories =
            filter isLegit allDirectories

      files <-
          filterM (doesFileExist . (directory </>)) directoryContents

      return (directories, files)


getReadme :: FilePath -> IO (Maybe String)
getReadme directory =
  do  exists <- doesFileExist (directory </> "README.md")
      if exists
        then
          Just `fmap` readFile (directory </> "README.md")
        else
          return Nothing

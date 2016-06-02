{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.Index (toHtml, getInfo, getPkg) where

import Control.Monad
import Control.Monad.Except (ExceptT, liftIO, runExceptT, throwError)
import Data.Aeson as Json
import qualified Data.ByteString.Lazy.UTF8 as BSU8
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Elm.Package as Pkg
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Paths as Paths
import qualified Elm.Package.Solution as S
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>), splitDirectories, takeExtension)
import qualified Text.Blaze.Html5 as H

import qualified Generate.Help as Help
import qualified StaticFiles



-- INFO


data Info = Info
    { _pwd :: [String]
    , _dirs :: [FilePath]
    , _files :: [(FilePath, Bool)]
    , _pkg :: Maybe PackageInfo
    , _readme :: Maybe String
    }


data PackageInfo = PackageInfo
    { _version :: Pkg.Version
    , _repository :: String
    , _summary :: String
    , _dependencies :: [(Pkg.Name, Pkg.Version)]
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


toHtml :: Info -> H.Html
toHtml info@(Info pwd _ _ _ _) =
  Help.makeHtml
    (List.intercalate "/" ("~" : pwd))
    ("/" ++ StaticFiles.indexPath)
    ("Elm.Index.fullscreen(" ++ BSU8.toString (Json.encode info) ++ ");")



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
    do
        desc <- getDescription

        solutionExists <- liftIO (doesFileExist Paths.solvedDependencies)
        when (not solutionExists) (throwError "file not found")
        solution <- S.read id Paths.solvedDependencies

        let publicSolution =
              Map.intersection solution (Map.fromList (Desc.dependencies desc))

        return $ PackageInfo
          { _version = Desc.version desc
          , _repository = Desc.repo desc
          , _summary = Desc.summary desc
          , _dependencies = Map.toList publicSolution
          }


getDescription :: ExceptT String IO Desc.Description
getDescription =
  do  descExists <- liftIO (doesFileExist Paths.description)
      when (not descExists) (throwError "file not found")
      Desc.read id Paths.description


getPkg :: IO Pkg.Name
getPkg =
  fmap
    (either (const Pkg.dummyName) Desc.name)
    (runExceptT getDescription)


getDirectoryInfo :: FilePath -> IO ([FilePath], [(FilePath, Bool)])
getDirectoryInfo directory =
  do  directoryContents <-
          getDirectoryContents directory

      allDirectories <-
          filterM (doesDirectoryExist . (directory </>)) directoryContents

      let isLegit name =
            List.notElem name [".", "..", "elm-stuff"]

      let directories =
            filter isLegit allDirectories

      rawFiles <-
          filterM (doesFileExist . (directory </>)) directoryContents

      files <- mapM (inspectFile directory) rawFiles

      return (directories, files)


inspectFile :: FilePath -> FilePath -> IO (FilePath, Bool)
inspectFile directory filePath =
  if takeExtension filePath == ".elm" then
    do  source <- Text.readFile (directory </> filePath)
        let hasMain = Text.isInfixOf "\nmain " source
        return (filePath, hasMain)

  else
    return (filePath, False)




getReadme :: FilePath -> IO (Maybe String)
getReadme directory =
  do  exists <- doesFileExist (directory </> "README.md")
      if exists
        then
          Just `fmap` readFile (directory </> "README.md")
        else
          return Nothing

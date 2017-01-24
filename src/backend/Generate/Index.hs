{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.Index (toHtml, getProject, moveToRoot) where

import Control.Monad (filterM)
import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.UTF8 as BSU8
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Directory as Dir
import System.FilePath ((</>), joinPath, splitDirectories, takeExtension)
import qualified Text.Blaze.Html5 as H

import qualified Elm.Config as Config
import qualified Generate.Help as Help
import qualified StaticFiles



-- PROJECT


data Project =
  Project
    { _root :: FilePath
    , _info :: PerhapsInfo
    }


data PerhapsInfo
  = Bad (Maybe FilePath)
  | Good Info


data Info =
  Info
    { _pwd :: [String]
    , _dirs :: [FilePath]
    , _files :: [(FilePath, Bool)]
    , _readme :: Maybe String
    , _config :: Text.Text
    }



-- JSON


goodToJson :: FilePath -> Info -> String
goodToJson root info =
  BSU8.toString $ Json.encode $ Json.object $
    [ "root" .= root
    , "pwd" .= _pwd info
    , "dirs" .= _dirs info
    , "files" .= _files info
    , "readme" .= _readme info
    , "config" .= _config info
    ]


badJson :: FilePath -> Maybe FilePath -> String
badJson root potentialRoot =
  BSU8.toString $ Json.encode $ Json.object $
    [ "root" .= root
    , "suggestion" .= potentialRoot
    ]



-- GENERATE HTML


toHtml :: Project -> H.Html
toHtml (Project root perhapsInfo) =
  case perhapsInfo of
    Good info@(Info pwd _ _ _ _) ->
      Help.makeHtml
        (List.intercalate "/" ("~" : pwd))
        ("/" ++ StaticFiles.indexPath)
        ("Elm.Index.fullscreen(" ++ goodToJson root info ++ ");")

    Bad suggestion ->
      Help.makeHtml
        (maybe "New Project!" (const "Existing Project?") suggestion)
        ("/" ++ StaticFiles.startPath)
        ("Elm.Start.fullscreen(" ++ badJson root suggestion ++ ");")



-- GET PROJECT


getProject :: FilePath -> IO Project
getProject directory =
  do  root <- Dir.getCurrentDirectory
      exists <- Dir.doesFileExist Config.path
      Project root <$>
        case exists of
          False ->
            Bad <$> findNearestConfig (splitDirectories root)

          True ->
            do  json <- Text.readFile Config.path
                Good <$> getInfo json directory


findNearestConfig :: [String] -> IO (Maybe FilePath)
findNearestConfig dirs =
  if null dirs then
    return Nothing

  else
    do  exists <- Dir.doesFileExist (joinPath dirs </> Config.path)
        if exists
          then return (Just (joinPath dirs))
          else findNearestConfig (init dirs)


moveToRoot :: IO ()
moveToRoot =
  do  subDir <- Dir.getCurrentDirectory
      maybeRoot <- findNearestConfig (splitDirectories subDir)
      case maybeRoot of
        Nothing ->
          return ()

        Just root ->
          Dir.setCurrentDirectory root



-- GET INFO


getInfo :: Text.Text -> FilePath -> IO Info
getInfo config directory =
  do  (dirs, files) <- getDirectoryInfo directory
      readme <- getReadme directory
      return $
        Info
          { _pwd = dropWhile ("." ==) (splitDirectories directory)
          , _dirs = dirs
          , _files = files
          , _readme = readme
          , _config = config
          }



-- README


getReadme :: FilePath -> IO (Maybe String)
getReadme dir =
  do  let readmePath = dir </> "README.md"
      exists <- Dir.doesFileExist readmePath
      if exists
        then Just <$> readFile readmePath
        else return Nothing



-- DIRECTORIES / FILES


getDirectoryInfo :: FilePath -> IO ([FilePath], [(FilePath, Bool)])
getDirectoryInfo dir =
  do  contents <- Dir.getDirectoryContents dir

      allDirs <- filterM (Dir.doesDirectoryExist . (dir </>)) contents
      rawFiles <- filterM (Dir.doesFileExist . (dir </>)) contents

      files <- mapM (inspectFile dir) rawFiles

      return (allDirs, files)


inspectFile :: FilePath -> FilePath -> IO (FilePath, Bool)
inspectFile dir filePath =
  if takeExtension filePath == ".elm" then
    do  source <- Text.readFile (dir </> filePath)
        let hasMain = Text.isInfixOf "\nmain " source
        return (filePath, hasMain)

  else
    return (filePath, False)

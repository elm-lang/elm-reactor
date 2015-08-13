{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Index (getInfo, toHtml) where

import Control.Monad
import Control.Monad.Except (runExceptT)
import Data.Aeson as Json
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.List as List
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Paths as Paths
import System.Directory ( doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ( (</>), splitDirectories )

import StaticFiles


-- INFO

data Info = Info
  { _pwd :: [String]
  , _dirs :: [FilePath]
  , _files :: [FilePath]
  , _description :: Maybe Desc.Description
  }


instance ToJSON Info where
  toJSON model =
    object
      [ "pwd" .= _pwd model
      , "dirs" .= _dirs model
      , "files" .= _files model
      , "description" .= _description model
      ]


-- GENERATE HTML

toHtml :: Info -> BSC.ByteString
toHtml model@(Info pwd _ _ _) =
  BSC.pack $ unlines $
    [ "<html>"
    , ""
    , "<head>"
    , "  <title>" ++ List.intercalate "/" ("~" : pwd) ++ "</title>"
    , "  <script src=\"/" ++ StaticFiles.indexPath ++ "\"></script>"
    , "  <style>html, body { margin: 0; padding: 0; }</style>"
    , "</head>"
    , ""
    , "<body>"
    , "  <script type=\"text/javascript\">"
    , "    Elm.fullscreen(Elm.Index, { info:"
    , "        " ++ LBSC.unpack (Json.encode model)
    , "    });"
    , "  </script>"
    , "</body>"
    , ""
    , "</html>"
    ]


-- GET INFO FOR THIS LOCATION

getInfo :: FilePath -> IO Info
getInfo directory =
  do  description <- getDescription
      (dirs, files) <- getDirectoryInfo directory
      return $ Info
          { _pwd = toPwd directory
          , _dirs = dirs
          , _files = files
          , _description = description
          }


toPwd :: FilePath -> [String]
toPwd directory =
  case splitDirectories directory of
    "." : path ->
        path

    path ->
        path


getDescription :: IO (Maybe Desc.Description)
getDescription =
  do  result <- runExceptT (Desc.read Paths.description)
      return (either (const Nothing) Just result)


getDirectoryInfo :: FilePath -> IO ([FilePath], [FilePath])
getDirectoryInfo directory =
  do  directoryContents <-
          getDirectoryContents directory

      allDirectories <-
          filterM (doesDirectoryExist . (directory </>)) directoryContents

      let isLegit name =
            List.notElem name [".", ".."]

      let directories =
            filter isLegit allDirectories

      files <-
          filterM (doesFileExist . (directory </>)) directoryContents

      return (directories, files)
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Index (elmIndexGenerator) where


import           Control.Arrow              ((***))
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as LBS
import           Data.List                  (intercalate)
import           Data.Aeson as Json
import qualified Data.Map as Map
import           Data.FileEmbed             (embedFile)
import           System.Directory           (doesDirectoryExist, doesFileExist,
                                            getDirectoryContents)
import           System.FilePath            ((</>), splitDirectories)
import           Snap.Core                  (MonadSnap, modifyResponse, setContentType, writeBS)
import           Utils                      (splitList)


elmConfig :: FilePath
elmConfig = "elm-package.json"


data Model = Model
  { currentFolder :: FilePath
  , folders       :: [FilePath]
  , files         :: [FilePath]
  , currpackage   :: Maybe ElmPackage
  }


data ElmPackage = ElmPackage
  { pversion :: String
  , license :: String
  , summary :: String
  , dependencies :: [Dependency]
  , repository :: String
  }


data Dependency = Dependency
  { name    :: String
  , dversion :: String
  , account :: String
  }


instance FromJSON ElmPackage where
  parseJSON (Object o) = ElmPackage
    <$> o .: "version"
    <*> o .: "license"
    <*> o .: "summary"
    <*> join (mapM (uncurry toDependency) . Map.toList <$> o .: "dependencies")
    <*> o .: "repository"
    where
      toDependency val cversion =
        case splitList '/' val of
          [cname, caccount] ->
            return Dependency { account = caccount
                              , dversion = cversion
                              , name    = cname
                              }
          _ -> mzero
  parseJSON _ = mzero


instance ToJSON ElmPackage where
  toJSON = object . sequenceA
    [ (.=) "version" . pversion
    , (.=) "license" . license
    , (.=) "summary" . summary
    , (.=) "dependencies" . dependencies
    , (.=) "repository" . repository
    ]


instance ToJSON Dependency where
  toJSON = object . sequenceA
    [ (.=) "version" . dversion
    , (.=) "name" . name
    , (.=) "account" . account
    ]


instance ToJSON Model where
  toJSON model = object
    [ "folders" .= folders model
    , "files" .= files model
    , maybe ("currpackage" .= Null) ("currpackage" .=) (currpackage model)
    , "currentFolder" .= currentFolder model
    ]


indexPageTemplate :: S.ByteString -> S.ByteString -> S.ByteString
indexPageTemplate directory projectData = S.intercalate ""
  [ "<head><title>"
  , directory
  , "</title><script charset=\"utf-8\">"
  , $(embedFile "embedded/elm.js")
  , "</script><script type=\"text/javascript\">"
  , projectData
  , "</script><style type=\"text/css\">"
  , $(embedFile "embedded/index.css")
  , "</style></head><body><script type=\"text/javascript\">window.addEventListener(\"DOMContentLoaded\", function (){ Elm.fullscreen(Elm.Index, { modelPort: document.initialModel });})</script></body>"
  ]


dataToJs :: Model -> S.ByteString
dataToJs model = LBS.toStrict $ "(function(){document.initialModel=" `LBS.append` encode model `LBS.append` "})();"


elmIndexGenerator :: MonadSnap m => FilePath -> m ()
elmIndexGenerator directory = do
  modifyResponse $ setContentType "text/html; charset=utf-8"

  let (title, currdir) =
        intercalate "/" *** intercalate "/" $
        case splitDirectories directory of
          "." : rest -> ("~" : rest, rest)
          path -> (path, path)

  entries <- liftIO $ getDirectoryContents directory
  allDirs <- liftIO $ filterM (doesDirectoryExist . (directory </>)) entries
  let filterFunc = if directory `elem` ["", ".", "./"] then not . (`elem` ["..", "."]) else (/= ".")
  let notDotted = filter filterFunc allDirs
  allFiles   <- liftIO $ filterM (doesFileExist . (directory </>)) entries
  configExists <- liftIO $ doesFileExist $ "." </> elmConfig
  config <- if configExists
    then decodeStrict <$> (liftIO $ S.readFile $ "." </> elmConfig)
    else return Nothing

  let model =
        Model
        { folders = notDotted
        , files = allFiles
        , currentFolder = currdir
        , currpackage = config
        }

  writeBS $ indexPageTemplate (S.pack title) (dataToJs model)

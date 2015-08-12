{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Index (elmIndexGenerator) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson as Json
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Map as Map
import Snap.Core ( MonadSnap, modifyResponse, setContentType, writeBS )
import System.Directory ( doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ( (</>), splitDirectories )
import Utils ( splitList )


elmConfig :: FilePath
elmConfig =
  "elm-package.json"


compiledCodeLocation :: BSC.ByteString
compiledCodeLocation =
  "/_reactor/index.js"


{-| Representation of the Model record as used by the frontend.
-}
data Model = Model
  { currentFolder :: FilePath
  , folders       :: [FilePath]
  , files         :: [FilePath]
  , currpackage   :: Maybe ElmPackage
  }


{-| Representation of an ElmPackage record as used by the frontend.
-}
data ElmPackage = ElmPackage
  { pversion     :: String
  , license      :: String
  , summary      :: String
  , dependencies :: [Dependency]
  , repository   :: String
  }


{-| Representation of a package Dependency record as used by the frontend.
-}
data Dependency = Dependency
  { name     :: String
  , dversion :: String
  , account  :: String
  }


{-| 'FromJSON' instance of 'ElmPackage' that parses the elm-package.json format.
-}
instance FromJSON ElmPackage where
  parseJSON (Object obj) =
      let
        toDependency val cversion =
          case splitList '/' val of
            [cname, caccount] ->
              return Dependency { account  = caccount
                                , dversion = cversion
                                , name     = cname
                                }
            _ -> mzero
      in
        ElmPackage
          <$> obj .: "version"
          <*> obj .: "license"
          <*> obj .: "summary"
          <*> join (mapM (uncurry toDependency) . Map.toList <$> obj .: "dependencies")
          <*> obj .: "repository"

  parseJSON _ = mzero


{-| Special 'ToJSON' instance that producec json as accepted by the elm-reactor frontend Elm code.
-}
instance ToJSON ElmPackage where
  toJSON pkg =
    object
      [ "version" .= pversion pkg
      , "license" .= license pkg
      , "summary" .= summary pkg
      , "dependencies" .= dependencies pkg
      , "repository" .= repository pkg
      ]


{-| Special 'ToJSON' instance that produces
-}
instance ToJSON Dependency where
  toJSON dep =
    object
      [ "version".= dversion dep
      , "name" .= name dep
      , "account" .= account dep
      ]


{-| Special 'ToJSON' instance that producec json as accepted by the elm-reactor frontend Elm code.
-}
instance ToJSON Model where
  toJSON model =
    object
      [ "folders" .= folders model
      , "files" .= files model
      , maybe ("currpackage" .= Null) ("currpackage" .=) (currpackage model)
      , "currentFolder" .= currentFolder model
      ]


{-| The webpage template for the reactor.
-}
indexPageTemplate :: BSC.ByteString -> BSC.ByteString -> BSC.ByteString
indexPageTemplate directory projectData =
  BSC.intercalate ""
    [ "<head><title>"
    , directory
    , "</title><script charset=\"utf-8\" src=\""
    , compiledCodeLocation
    , "\"></script><script type=\"text/javascript\">"
    , projectData
    , "</script><style>html, body {margin: 0;padding: 0;}</style>\
      \</head><body><script type=\"text/javascript\">\
      \window.addEventListener(\"DOMContentLoaded\", function (){ \
      \Elm.fullscreen(Elm.Index, { modelPort: document.initialModel });})\
      \</script></body>\n"
    ]


{-| Turn a model into the javascript script that provides data to the elm code.
-}
dataToJs :: Model -> BSC.ByteString
dataToJs model =
  LBS.toStrict $
      LBS.concat
        [ "(function(){document.initialModel="
        , encode model
        , "})();"
        ]


elmIndexGenerator :: MonadSnap m => FilePath -> m ()
elmIndexGenerator directory =
  do  modifyResponse $ setContentType "text/html; charset=utf-8"

      let (title, currdir) =
            List.intercalate "/" *** List.intercalate "/" $
              case splitDirectories directory of
                "." : rest ->
                    ("~" : rest, rest)

                path ->
                    (path, path)

      entries <- liftIO $ getDirectoryContents directory
      allDirs <- liftIO $ filterM (doesDirectoryExist . (directory </>)) entries
      let filterFunc = if directory `elem` ["", ".", "./"] then not . (`elem` ["..", "."]) else (/= ".")
      let notDotted = filter filterFunc allDirs
      allFiles <- liftIO $ filterM (doesFileExist . (directory </>)) entries
      configExists <- liftIO $ doesFileExist $ "." </> elmConfig
      config <- if configExists
        then decodeStrict <$> liftIO (BSC.readFile $ "." </> elmConfig)
        else return Nothing

      let model =
            Model
            { folders = notDotted
            , files = allFiles
            , currentFolder = currdir
            , currpackage = config
            }

      writeBS $ indexPageTemplate (BSC.pack title) (dataToJs model)

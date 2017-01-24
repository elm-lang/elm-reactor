module Index.Project exposing
  ( Project
  , File(..)
  , decode
  , Error
  )


import Json.Decode as Decode exposing (..)

import Elm.Config as Config exposing (Config)




-- PROJECT


type alias Project =
  { root : String
  , pwd : List String
  , dirs : List String
  , files : List File
  , config : Config
  , readme : Maybe String
  }


type File
  = HasMain String
  | NoMain String
  | Other String



-- DECODE JSON


decode : Decode.Value -> Result Error Project
decode json =
  case decodeValue decoder json of
    Err msg ->
      Debug.crash ("bug in elm-reactor: " ++ msg)

    Ok realResult ->
      realResult


type alias Error =
  { root : String
  , hint : String
  }



-- DECODER


decoder : Decoder (Result Error Project)
decoder =
  succeed validateJson
    & field "root" string
    & field "pwd" (list string)
    & field "dirs" (list string)
    & field "files" (list fileDecoder)
    & field "readme" (nullable string)
    & field "config" string


(&) : Decoder (a -> b) -> Decoder a -> Decoder b
(&) =
  map2 (<|)


fileDecoder : Decoder (String, Bool)
fileDecoder =
  map2 (,) (index 0 string) (index 1 bool)



-- VALIDATE JSON


validateJson
  : String
  -> List String
  -> List String
  -> List (String, Bool)
  -> Maybe String
  -> String
  -> Result Error Project
validateJson root pwd rawDirs rawFiles readme rawConfig =
  case decodeString Config.decoder rawConfig of
    Err hint ->
      Err (Error root hint)

    Ok config ->
      Ok
        { root = root
        , pwd = pwd
        , dirs = List.filter (isGoodDir config) rawDirs
        , files = List.filterMap toFile rawFiles
        , readme = readme
        , config = config
        }


isGoodDir : Config -> String -> Bool
isGoodDir config dir =
  not (String.startsWith "." dir)
  &&
  case config of
    Config.App {outputDirectory} ->
      dir /= outputDirectory && dir /= "elm-stuff"

    Config.Pkg _ ->
      dir /= "elm-stuff"


toFile : (String, Bool) -> Maybe File
toFile (name, hasMain) =
  if String.startsWith "." name then
    Nothing

  else if hasMain then
    Just (HasMain name)

  else if String.endsWith ".elm" name then
    Just (NoMain name)

  else
    Just (Other name)

module UserProgram exposing
  ( ElmValue
  , UserProgram
  )
  -- where


import Html exposing (Html)
import Json.Decode as Json


type alias ElmValue =
  Json.Value


type alias UserProgram =
  { init : (ElmValue, Cmd ElmValue)
  , step : ElmValue -> ElmValue -> (ElmValue, Cmd ElmValue)
  , subs : ElmValue -> Sub ElmValue
  , view : ElmValue -> Html ElmValue
  }

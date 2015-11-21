module Explorer.Value.Test where

import Json.Encode as Json exposing (..)
import StartApp.Simple as StartApp

import Explorer.Value.Expando as Expando
import Explorer.Value.FromJs exposing (ElmValue, toElmValue)


main =
  StartApp.start
    { model = Expando.init dummy
    , view = Expando.view
    , update = Expando.update
    }


dummy : ElmValue
dummy =
  toElmValue (float 42.123491827450987)



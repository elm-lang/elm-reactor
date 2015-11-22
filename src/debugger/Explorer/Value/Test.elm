module Explorer.Value.Test where

import Json.Encode as Json exposing (..)
import StartApp.Simple as StartApp

import Explorer.Value.Expando as Expando
import Explorer.Value.FromJs exposing (ElmValue, toElmValue, unsafeCast)


main =
  StartApp.start
    { model = Expando.init dummy
    , view = Expando.view
    , update = Expando.update
    }


dummy : ElmValue
dummy =
  toElmValue (unsafeCast [{x=2,y="sup"}, {x=3, y="bloop"}])

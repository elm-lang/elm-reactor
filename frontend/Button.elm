module Button where

import Signal
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)


type Model
    = Up
    | Down
    | Hover


type Action
    = MouseUpdate Model


update : Action -> Model -> Model
update action state =
  case action of
    MouseUpdate newState ->
      newState


view : Signal.Address Action
    -> Signal.Address a
    -> a
    -> Model
    -> (Model -> Html)
    -> Html
view buttonStateAddr actionAddr action state render =
  div
    [ onMouseOver buttonStateAddr (MouseUpdate Hover)
    , onMouseDown buttonStateAddr (MouseUpdate Down)
    , onMouseUp buttonStateAddr (MouseUpdate Hover)
    , onMouseLeave buttonStateAddr (MouseUpdate Up)
    , onClick actionAddr action
    ]
    [ lazy render state ]

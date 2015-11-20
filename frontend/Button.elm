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


type Message a
    = MouseUpdate Model
    | Click a


update : Message a -> Model -> (Model, Maybe a)
update msg state =
  case msg of
    MouseUpdate newState ->
      (newState, Nothing)

    Click clickMsg ->
      (state, Just clickMsg)


view
    : Signal.Address (Message a)
    -> a
    -> Model
    -> (Model -> Html)
    -> Html
view addr clickMsg state render =
  div
    [ onMouseEnter addr (MouseUpdate Hover)
    , onMouseDown addr (MouseUpdate Down)
    , onMouseUp addr (MouseUpdate Hover)
    , onMouseLeave addr (MouseUpdate Up)
    , onClick addr (Click clickMsg)
    ]
    [ lazy render state
    ]


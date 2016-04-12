port module Debugger exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json

import History exposing (History)
import UserProgram exposing (ElmValue, UserProgram)



main : Program Never
main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


port changes : (Json.Value -> msg) -> Sub msg



-- MODEL


type Model
  = Loading
  | Error (Maybe Session)
  | Playing Session
  | Paused Int Session


type alias Session =
  { program : UserProgram
  , history : History
  , currentUserModel : ElmValue
  }


init : (Model, Cmd Msg)
init =
  ( Loading, Cmd.none )



-- UPDATE


type Msg
  = UserMessage History.Source ElmValue
  | Blocked
  | Changes Json.Value


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    _ ->
      ( Debug.log "model" model
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  changes Changes



-- VIEW


(=>) = (,)


view : Model -> Html Msg
view model =
  div [ style outerDivStyles ] <|
    case model of
      Loading ->
        [ loadingMessage
        ]

      Error maybeSession ->
        [
        ]

      Playing session ->
        [
        ]

      Paused time session ->
        [ blocker
        ]


outerDivStyles : List (String, String)
outerDivStyles =
  [ "display" => "block"
  , "position" => "relative"
  ]



-- LOADING


loadingMessage : Html msg
loadingMessage =
  div
    [ style
        [ "width" => "100%"
        , "height" => "100%"
        , "display" => "flex"
        , "flex-direction" => "column"
        , "justify-content" => "center"
        , "align-items" => "center"
        , "color" => "#9A9A9A"
        , "font-family" => "Source Sans Pro"
        ]
    ]
    [ div [ style ["font-size" => "3em"] ] [ text "Building your project!" ]
    , img [ src "/_reactor/waiting.gif" ] []
    , div [ style ["font-size" => "1em"] ] [ text "With new projects, I need a bunch of extra time to download packages." ]
    ]



-- PAUSED


blocker : Html Msg
blocker =
  let
    variousEvents =
      [ "click", "mousemove", "mouseup", "mousedown", "mouseclick", "keydown"
      , "keypress", "keyup", "touchstart", "touchend", "touchcancel", "touchleave"
      , "touchmove", "pointermove", "pointerdown", "pointerup", "pointerover"
      , "pointerout", "pointerenter", "pointerleave", "pointercancel"
      ]

    block eventName =
      on eventName (Json.succeed Blocked)

    styleStuff =
      [ "position" => "absolute"
      , "width" => "100%"
      , "height" => "100%"
      , "background-color" => "rgba(100, 100, 100, 0.3)"
      ]
  in
    div (style styleStuff :: List.map block variousEvents) []

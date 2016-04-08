module Debugger exposing (main) -- where

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)

import SideBar as SB
import History
import UserProgram



main : Program Setup
main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model
  { setup : Setup
  , state : State
  }


type State
  = Loading
  | Error (Maybe Session)
  | Playing Session
  | Paused Int Session


type alias Session =
  { program : UserProgram
  , history : History
  , currentUserModel : ElmValue
  }


type alias Setup =
  { flags : Json.Value
  , file : String
  , host : String
  }


init : Setup -> (Model, Cmd Msg)
init setup =
  ( Model setup Loading, Cmd.none )



-- UPDATE


type Msg
  = UserMessage History.Source ElmValue
  | Controls SB.Msg
  | Blocked


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


(=>) = (,)


view : Model -> Html Msg
view model =
  div [ style outerDivStyles ] <|
    case model.state of
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
        ]
    ]
    [ div [ style ["font-size" => "3em"] ] [ text "Your code is compiling..." ]
    , img [ src "_reactor/waiting.gif" ] []
    , div [ style ["font-size" => "1em"] ] [ text "With new projects, I need a bunch of extra time to download packages." ]
    ]



-- PAUSED


blocker : Html msg
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

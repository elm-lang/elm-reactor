module Main exposing (..) -- where

import History
import UserProgram



-- MODEL


type alias Model =
  { program : UserProgram
  , history : History
  , status : Status
  , userModel : ElmValue
  }


type Status
  = Paused Int
  | Playing



-- UPDATE


type Msg
  = UserMsg ElmValue
  | ControlsMsg Controls.Msg
  | Blocked



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


(=>) = (,)


view : Model -> Html Msg
view model =
  div [ style outerDivStyles ] <|
    case model.status of
      Playing ->
        [ program.view model.userModel
        ]

      Paused time ->
        [ program.view (History.timeTravel time model.history)
        , blocker
        ]


outerDivStyles : List (String, String)
outerDivStyles =
  [ "display" => "block"
  , "position" => "relative"
  ]


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

module SideBar.Controls where

import Color
import FontAwesome
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import String

import Debugger.Active as Active
import Model
import SideBar.Button as Button
import Utils.Style exposing ((=>))


styles : String
styles = """

.controls {
  display: -webkit-flex;
  -webkit-flex-direction: row;
  -webkit-justify-content: space-between;
  -webkit-align-items: center;
  padding: 16px;
}

.left-sidebar-header {
  -webkit-flex: 0 0 auto;
  -moz-flex: 0 0 auto;
  -ms-flex: 0 0 auto;
  flex: 0 0 auto;
}

.scrubber {
  margin: 0;
}

"""


-- VIEW

view : Signal.Address Model.Message -> Model.Model -> Active.Model -> Html
view addr state activeState =
  let
    buttonContainer =
      div
        [ class "controls" ]
        [ playPauseButton
            (Signal.forwardTo addr Model.PlayPauseButtonAction)
            (not <| Active.isPlaying activeState)
            state.playPauseButtonState
        ]

    sliderContainer =
      div
        [ class "scrubber-container"]
        [ scrubSlider (Signal.forwardTo addr Model.ServiceCommand) activeState
        ]
  in
    div
      [ class "left-sidebar-header" ]
      [ buttonContainer
      , sliderContainer
      ]


playPauseButton : Signal.Address (Button.Message Active.Command) -> Bool -> Button.Model -> Html
playPauseButton addr isPlay state =
  let
    icon =
      if isPlay then
        FontAwesome.play Color.black 30
      else
        FontAwesome.pause Color.black 30

    render state =
      icon
  in
    Button.view
      addr
      (if isPlay then Active.Play else Active.Pause)
      state
      render


scrubSlider : Signal.Address Active.Command -> Active.Model -> Html
scrubSlider addr activeState =
  let
    numFrames =
      activeState.numFrames

    curFrame =
      Active.curFrameIdx activeState
  in
    input
      [ type' "range"
      , class "scrubber"
      , Attr.min "0"
      , Attr.max <| toString <| numFrames - 1
      , Attr.value <| toString <| curFrame
      , on "input"
          (at ["target","value"] (customDecoder string String.toInt))
          (\idx ->
            Signal.message
              addr
              (Active.ScrubTo idx))
      ]
      []


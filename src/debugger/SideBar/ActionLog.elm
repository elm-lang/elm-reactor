module SideBar.ActionLog where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Debugger.RuntimeApi as API
import Debugger.Model as DM
import Utils.Style exposing ((=>))


type Message
    = GoToFrame DM.FrameIndex


styles : String
styles = """

.action-log {
  background-color: lightgrey;
  margin: 0;

  -webkit-flex: 0 1 auto;
  -moz-flex: 0 1 auto;
  -ms-flex: 0 1 auto;
  flex: 0 1 auto;

  overflow-y: scroll;
}

"""


view : Signal.Address Message -> DM.ValueLog -> DM.FrameIndex -> Html
view addr actions curFrameIdx =
  ul
    [ class "action-log" ]
    (List.map (viewAction addr curFrameIdx) actions)


viewAction
    : Signal.Address Message
    -> DM.FrameIndex
    -> (DM.FrameIndex, DM.JsElmValue)
    -> Html
viewAction addr curFrameIdx (frameIdx, value) =
  let
    onThisFrame =
      curFrameIdx == frameIdx
  in
    li
      [ onClick addr (GoToFrame frameIdx)
      , style
          [ "color" => if onThisFrame then "red" else "black"
          , "cursor" => "pointer"
          , "font-family" => "monospace"
          ]
      ]
      [ text (API.prettyPrint value)
      ]

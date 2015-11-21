module SideBar.ActionLog where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Debugger.RuntimeApi as API
import Debugger.Model as DM
import Utils.Style exposing ((=>))
import Json.Decode as JsDec


type Message
    = GoToFrame DM.FrameIndex
    | ScrollLogs


styles : String
styles = """

#action-log {
  background-color: lightgrey;
  margin: 0;

  -webkit-flex: 0 1 auto;
  -moz-flex: 0 1 auto;
  -ms-flex: 0 1 auto;
  flex: 0 1 auto;

  flex-grow: 1;

  background-color: rgb(61, 61, 61);

  overflow-y: scroll;
}

.action-log-entry {
  cursor: pointer;
  font-family: monospace;
  color: rgb(99, 99, 99);
  padding: 4px 8px;
}

.action-log-entry:hover {
  background-color: rgb(41, 41, 41);
  color: rgb(164, 164, 164);
  padding: 4px 8px;
}

.action-log-entry-active {
  color: rgb(224, 224, 224);
}
"""


view : Signal.Address Message -> DM.ValueLog -> DM.FrameIndex -> Html
view addr actions curFrameIdx =
  div
    [ id "action-log"
    , on "scroll" (JsDec.succeed ()) (\_ -> Signal.message addr ScrollLogs)
    ]
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
    div
      [ onClick addr (GoToFrame frameIdx)
      , classList
          [ "action-log-entry" => True
          , "action-log-entry-active" => onThisFrame
          ]
      ]
      [ text (API.prettyPrint value)
      ]

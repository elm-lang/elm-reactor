module SideBar.ActionLog where

import Json.Decode as JsDec
import Dict

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Debugger.RuntimeApi as API
import Debugger.Model as DM
import Debugger.Active as Active
import Utils.Style exposing ((=>))
import Utils.Helpers exposing (unsafe)
import Explorer.Value.Expando exposing (Expando)
import Explorer.Value.FromJs exposing (ElmValue)


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


view : Signal.Address Message -> Active.Model -> Html
view addr activeState =
  let
    curFrameIdx =
      Active.curFrameIdx activeState

    actionsNodeExpandoLog =
      case activeState.salientNodes.foldps of
        [{parent, foldp}] ->
          Dict.get parent activeState.nodeLogs
            |> Maybe.withDefault []

        -- no foldps
        [] ->
          []

        _ ->
          Debug.crash "multiple foldps"
  in
    div
      [ id "action-log"
      , on "scroll" (JsDec.succeed ()) (\_ -> Signal.message addr ScrollLogs)
      ]
      (List.map (viewAction addr curFrameIdx) actionsNodeExpandoLog)


viewAction
    : Signal.Address Message
    -> DM.FrameIndex
    -> (DM.FrameIndex, (ElmValue, Expando))
    -> Html
viewAction addr curFrameIdx (frameIdx, (elmValue, expando)) =
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
      [ text (toString elmValue) -- TODO: collapsed version
      ]

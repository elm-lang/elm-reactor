module Debugger.Active where

import Components exposing (..)
import Dict
import Set
import Html exposing (Html)
import Debugger.Reflect as Reflect
import Debug

import Debugger.RuntimeApi as API
import DataUtils exposing (..)


-- TODO: rename?
type alias Model =
  { session : API.DebugSession
  , runningState : RunningState
  , swapState : SwapState
  , mainVal : Html
  , exprLogs : Dict.Dict API.ExprTag API.ValueLog
  -- vv TODO: get inputs for each frame as well
  , nodeLogs : Dict.Dict API.NodeId API.ValueLog
  , subscribedNodes : Set.Set API.NodeId
  }


initModel : API.DebugSession -> Html -> Model
initModel session mainVal =
  { session = session
  , runningState = Playing
  , swapState = NotSwapping
  , mainVal = mainVal
  , exprLogs = Dict.empty
  , nodeLogs = Dict.empty
  , subscribedNodes = Set.empty
  }


type SwapState
  = Swapping
  | NotSwapping
  | SwapFailed SwapError


type RunningState
  = Playing
  | Paused Int


type alias SwapError =
  String


type Message
  = Command Command
  | Notification Notification


type Command
  = Play
  | Pause
  | ScrubTo API.FrameIndex
  | Reset
  | NoOpCommand


type Notification
  = NewFrame API.NewFrameNotification
  -- TODO: task update
  | NoOpNot


update : Message -> Model -> Transaction Message Model
update msg state =
  case msg of
    Command cmd ->
      case cmd of
        Play ->
          Debug.crash "Play not implemented yet"

        Pause ->
          Debug.crash "Pause not implemented yet"

        ScrubTo frameIdx ->
          Debug.crash "ScrubTo not implemented yet"

        Reset ->
          Debug.crash "Reset not implemented yet"

    Notification not ->
      case not of
        NewFrame newFrameNot ->
          let
            newMainVal =
              newFrameNot.subscribedNodeValues
                |> getMainVal state.session
          in
            done
              { state | mainVal <- newMainVal }

        NoOpNot ->
          done state


getMainValFromLogs : API.DebugSession -> List (Int, API.ValueLog) -> Html
getMainValFromLogs session logs =
  let
    mainId =
      (API.sgShape session).mainId

  in
    logs
      |> List.filter (\(id, val) -> id == mainId)
      |> List.head
      |> getMaybe "no log with main id"
      |> snd -- value log
      |> List.head
      |> getMaybe "log empty"
      |> snd -- js elm value
      |> Reflect.getHtml


getMainVal : API.DebugSession -> API.ValueSet -> Html
getMainVal session values =
  let
    mainId =
      (API.sgShape session).mainId

  in
    values
      |> List.filter (\(id, val) -> id == mainId)
      |> List.head
      |> getMaybe "no value with main id"
      |> snd
      |> Reflect.getHtml


appendToLog : API.FrameIndex -> API.JsElmValue -> Maybe API.ValueLog -> Maybe API.ValueLog
appendToLog curFrame value maybeLog =
  let
    pair =
      (curFrame, value)

    newLog =
      case maybeLog of
        Just log ->
          log ++ [pair]

        Nothing ->
          [pair]
  in
    Just newLog


updateLogs : API.FrameIndex
          -> Dict.Dict comparable API.ValueLog
          -> List (comparable, API.JsElmValue)
          -> (comparable -> Bool)
          -> Dict.Dict comparable API.ValueLog
updateLogs curFrame logs updates idPred =
  List.foldl
    (\(tag, value) logs ->
      Dict.update tag (appendToLog curFrame value) logs)
    logs
    (List.filter (fst >> idPred) updates)


isPlaying : Model -> Bool
isPlaying model =
  case model.runningState of
    Playing ->
      True

    _ ->
      False


mainId : Model -> API.NodeId
mainId model =
  (API.sgShape model.session).mainId


numFrames : Model -> Int
numFrames model =
  API.numFrames model.session


curFrameIdx : Model -> Int
curFrameIdx model =
  case model.runningState of
    Paused idx ->
      idx

    _ ->
      numFrames model - 1

module Debugger.Model where

import Dict
import Set
import Html exposing (Html)
import Debug
import Debugger.Reflect as Reflect

import Debugger.RuntimeApi as API
import DataUtils exposing (..)


type Model
  = Uninitialized
  | Initializing
  | Active ActiveAttrs


-- TODO: rename?
type alias ActiveAttrs =
  { session : API.DebugSession
  , runningState : RunningState
  , swapState : SwapState
  , mainVal : Html
  , exprLogs : Dict.Dict API.ExprTag API.ValueLog
  -- vv TODO: get inputs for each frame as well
  , nodeLogs : Dict.Dict API.NodeId API.ValueLog
  , subscribedNodes : Set.Set API.NodeId
  }


initialActiveAttrs : API.DebugSession -> Html -> ActiveAttrs
initialActiveAttrs session mainVal =
  { session = session
  , runningState = Paused
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


type Notification
  = NewFrame API.NewFrameNotification
  -- TODO: task update
  | NoOpNot


type Action
  = Notification Notification
  | Command Command
  | Response Response



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


isPlaying : ActiveAttrs -> Bool
isPlaying activeAttrs =
  case activeAttrs.sessionState of
    Playing _ ->
      True

    Pausing ->
      True

    _ ->
      False


mainId : ActiveAttrs -> API.NodeId
mainId activeAttrs =
  (API.sgShape activeAttrs.session).mainId


numFrames : ActiveAttrs -> Int
numFrames activeAttrs =
  API.numFrames activeAttrs.session


curFrameIdx : ActiveAttrs -> Int
curFrameIdx activeAttrs =
  case activeAttrs.sessionState of
    Paused idx _ ->
      idx

    _ ->
      numFrames activeAttrs - 1

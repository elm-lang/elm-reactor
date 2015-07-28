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
  , sessionState : SessionState
  , mainVal : Html
  , exprLogs : Dict.Dict API.ExprTag API.ValueLog
  , nodeLogs : Dict.Dict API.NodeId API.ValueLog
  , subscribedNodes : Set.Set API.NodeId
  }


initialActiveAttrs : API.DebugSession -> Html -> ActiveAttrs
initialActiveAttrs session mainVal =
  { session = session
  , sessionState = Playing Nothing
  , mainVal = mainVal
  , exprLogs = Dict.empty
  , nodeLogs = Dict.empty
  , subscribedNodes = Set.empty
  }


type SessionState
  = Playing (Maybe RunningCmd)
  | Paused Int (Maybe RunningCmd)
  | Forking API.FrameIndex Bool
  | AlmostPlaying
  | Pausing
  | SwapError SwapError


type RunningCmd
  = Swapping
  | GettingNodeState API.FrameInterval
  | Subscribing Bool


-- maybe don't need to reify these after all
type Command
  = Initialize API.ElmModule
  | Pause
  | ForkFrom API.FrameIndex Bool
  | Subscribe API.NodeId Bool
  | GetNodeState API.FrameInterval (List API.NodeId)
  | Swap API.ElmModule
  | NoOpCommand


type Response
  = IsActive API.DebugSession API.ValueSet
  | IsSubscribed (Maybe API.ValueLog)
  | HasForked API.ValueSet
  | IsPlaying
  | IsPaused (Maybe API.FrameInterval)
  | SwapResult (Result SwapError API.ValueSet)
  | GotNodeState (List (API.NodeId, API.ValueLog))


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

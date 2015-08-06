module Debugger.Active where

import Dict exposing (Dict)
import Set exposing (Set)
import Html exposing (Html)
import Debug
import Task

import Transaction exposing (..)

import Debugger.RuntimeApi as API
import DataUtils exposing (..)
import Debugger.Reflect as Reflect


type alias Model =
  { session : API.DebugSession
  , runningState : RunningState
  , mainVal : Html
  , exprLogs : Dict API.ExprTag API.ValueLog
  -- vv TODO: get inputs for each frame as well
  , nodeLogs : Dict API.NodeId API.ValueLog
  , subscribedNodes : Set API.NodeId
  }


initModel : API.DebugSession -> Html -> Model
initModel session mainVal =
  { session = session
  , runningState = Playing
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
  | Response Response
  | NoOp


type Command
  = Play
  | Pause
  | ScrubTo API.FrameIndex
  | Reset
  | Swap API.CompiledElmModule
  | StartWithHistory API.InputHistory
  | NoOpCommand


type Notification
  = NewFrame API.NewFrameNotification
  -- TODO: task update
  | NoOpNot


type Response
  = ScrubResponse Html
  | ForkResponse API.DebugSession Html
  | SwapResponse API.DebugSession Html
  | StartWithHistoryResponse API.DebugSession API.ValueSet


update : Message -> Model -> Transaction Message Model
update msg state =
  case msg of
    Command cmd ->
      case cmd of
        Play ->
          case state.runningState of
            Paused pausedIdx ->
              let
                fork =
                  API.forkFrom state.session pausedIdx
                    |> Task.map (\(sesh, values) ->
                        Response <|
                          ForkResponse sesh (getMainVal state.session values))
              in
                requestTask fork { state | runningState <- Playing }

            Playing ->
              Debug.crash "already playing"

        Pause ->
          requestTask
            (API.setPlaying state.session False
              |> Task.map (always NoOp)
              |> Task.mapError (\_ -> Debug.crash "already in that state"))
            { state | runningState <- Paused (numFrames state - 1) }

        ScrubTo frameIdx ->
          let
            pause =
              case state.runningState of
                Playing ->
                  API.setPlaying state.session False
                    |> Task.map (always NoOp)
                    |> Task.mapError (\_ -> Debug.crash "already in that state")

                Paused _ ->
                  Task.succeed NoOp

            getState =
              API.getNodeStateSingle
                state.session
                frameIdx
                [(API.sgShape state.session).mainId]
              |> Task.map (Response << ScrubResponse << getMainVal state.session)

            sequenced =
              pause `Task.andThen` (always getState)
          in
            requestTask sequenced { state | runningState <- Paused frameIdx }

        Reset ->
          requestTask
            (API.forkFrom state.session 0
              |> Task.map (\(sesh, values) ->
                    Response <|
                      ForkResponse sesh (getMainVal state.session values)))
            { state |
                runningState <-
                  case state.runningState of
                    Playing ->
                      Playing

                    Paused _ ->
                      Paused 0
            }

        Swap compiledMod ->
          let
            newMod =
              API.evalModule compiledMod

            swapTask =
              (API.swap state.session newMod API.justMain
                |> Task.mapError (\swapErr -> Debug.crash "TODO"))
              `Task.andThen` (\newSesh ->
                API.getNodeStateSingle
                  newSesh
                  (curFrameIdx state)
                  [API.sgShape newSesh |> .mainId]
                |> Task.map (\values ->
                      Response <| SwapResponse newSesh (getMainVal newSesh values))
              )
          in
            requestTask swapTask state

        StartWithHistory hist ->
          let
            initTask =
              API.initializeFullscreen
                (API.getModule state.session)
                hist
                (API.getAddress state.session)
                (API.justMain)
              |> Task.map (\(newSesh, vals) ->
                Response (StartWithHistoryResponse newSesh vals))
          in
            requestTask initTask state

    Notification not ->
      case not of
        NewFrame newFrameNot ->
          let
            newMainVal =
              newFrameNot.subscribedNodeValues
                |> getMainVal state.session

            curFrame =
              curFrameIdx state

            mainNodeId =
              mainId state

            newExprLogs =
              updateLogs
                curFrame
                state.exprLogs
                newFrameNot.flaggedExprValues
                (always True)

            newNodeLogs =
              updateLogs
                curFrame
                state.nodeLogs
                newFrameNot.subscribedNodeValues
                (\id -> id /= mainNodeId)
          in
            done
              { state
                  | mainVal <- newMainVal
                  , exprLogs <- newExprLogs
                  , nodeLogs <- newNodeLogs
              }

        NoOpNot ->
          done state

    Response resp ->
      case resp of
        ScrubResponse html ->
          done { state | mainVal <- html }

        ForkResponse newSesh html ->
          let
            frameIdx =
              API.numFrames newSesh - 1
          in
            done
              { state
                  | session <- newSesh
                  , mainVal <- html
                  , exprLogs <- truncateLogs frameIdx state.exprLogs
                  , nodeLogs <- truncateLogs frameIdx state.nodeLogs
              }

        SwapResponse newSesh html ->
          done
            { state
                | session <- newSesh
                , mainVal <- html
            }

        StartWithHistoryResponse newSesh vals ->
          done
            { state
                | session <- newSesh
                , mainVal <- getMainVal newSesh vals
            }

    NoOp ->
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
          -> Dict comparable API.ValueLog
          -> List (comparable, API.JsElmValue)
          -> (comparable -> Bool)
          -> Dict comparable API.ValueLog
updateLogs curFrame logs updates idPred =
  List.foldl
    (\(tag, value) logs ->
      Dict.update tag (appendToLog curFrame value) logs)
    logs
    (List.filter (fst >> idPred) updates)


truncateLogs : API.FrameIndex -> Dict comparable API.ValueLog -> Dict comparable API.ValueLog
truncateLogs frameIdx logs =
  logs
    |> Dict.map (\_ log -> truncateLog frameIdx log)
    |> Dict.filter (\_ log -> not (List.isEmpty log))


truncateLog : API.FrameIndex -> API.ValueLog -> API.ValueLog
truncateLog frameIdx log =
  List.filter (\(idx, val) -> idx <= frameIdx) log


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


curFrameIdx : Model -> API.FrameIndex
curFrameIdx model =
  case model.runningState of
    Paused idx ->
      idx

    _ ->
      numFrames model - 1

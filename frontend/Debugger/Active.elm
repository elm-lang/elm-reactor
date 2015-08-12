module Debugger.Active where

import Dict exposing (Dict)
import Set exposing (Set)
import Html exposing (Html)
import Debug
import Task

import Transaction exposing (..)

import Debugger.RuntimeApi as API
import DataUtils exposing (..)


type alias Model =
  { session : API.DebugSession
  , runningState : RunningState
  , numFrames : Int
  , exprLogs : Dict API.ExprTag API.ValueLog
  -- vv TODO: get inputs for each frame as well
  , nodeLogs : Dict API.NodeId API.ValueLog
  , subscribedNodes : Set API.NodeId
  }


initModel : API.DebugSession -> Model
initModel session =
  { session = session
  , runningState = Playing
  , numFrames = 1
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
  = ScrubResponse API.JsElmValue
  | ForkResponse API.DebugSession API.FrameIndex API.JsElmValue
  | SwapResponse
      API.DebugSession
      API.JsElmValue
      (List (API.ExprTag, API.ValueLog))
  | StartWithHistoryResponse
      API.DebugSession
      API.JsElmValue
      Int
      (List (API.ExprTag, API.ValueLog))


update : Message -> Model -> Transaction Message Model
update msg state =
  case Debug.log "ACTIVE MSG" msg of
    Command cmd ->
      case cmd of
        Play ->
          case state.runningState of
            Paused pausedIdx ->
              let
                fork =
                  API.forkFrom state.session pausedIdx
                    |> Task.map (\(session, values) ->
                        Response <|
                          ForkResponse
                            session
                            pausedIdx
                            (getMainVal state.session values))
              in
                requestTask fork { state | runningState <- Playing }

            Playing ->
              Debug.crash "already playing"

        Pause ->
          requestTask
            (API.setPlaying state.session False
              |> Task.map (always NoOp)
              |> Task.mapError (\_ -> Debug.crash "already in that state"))
            { state | runningState <- Paused (state.numFrames - 1) }

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
                |> Task.mapError (Debug.crash << toString)
          in
            requestTask sequenced { state | runningState <- Paused frameIdx }

        Reset ->
          requestTask
            (API.forkFrom state.session 0
              |> Task.map (\(session, values) ->
                    Response <|
                      ForkResponse session 0 (getMainVal state.session values)))
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
            swapTask =
              (API.instantiateModule compiledMod)
              `Task.andThen` (\newMod ->
                (API.swap state.session newMod API.justMain
                  |> Task.toResult)
                `Task.andThen` (\swapRes ->
                  case swapRes of
                    Err replayError ->
                      Signal.send
                        (commandResponseMailbox ()).address
                        (SwapReplayError replayError)
                      |> Task.map (always NoOp)

                    Ok (newSession, logs) ->
                      API.getNodeStateSingle
                        newSession
                        (curFrameIdx state)
                        [API.sgShape newSession |> .mainId]
                      |> Task.mapError (Debug.crash << toString)
                      |> Task.map (\values ->
                            Response <|
                              SwapResponse
                                newSession
                                (getMainVal newSession values)
                                logs)
                )
              )
          in
            requestTask swapTask state

        StartWithHistory history ->
          let
            currentModule =
              API.getModule state.session

            currentModuleName =
              currentModule.name

            historyModuleName =
              API.getHistoryModuleName history

            initTask =
              if currentModuleName /= historyModuleName then
                let
                  error =
                    { currentModuleName = currentModule.name
                    , historyModuleName = API.getHistoryModuleName history
                    }
                in
                  Signal.send
                    (commandResponseMailbox ()).address
                    (HistoryMismatchError error)
                  |> Task.map (always NoOp)
              else
                (API.dispose state.session)
                `Task.andThen` (\_ ->
                  (API.initializeFullscreenAndSubscribe
                    currentModule
                    (API.getAddress state.session)
                    API.justMain)
                  `Task.andThen` (\(newSession, _) ->
                    (API.setInputHistory
                      newSession
                      history
                    |> Task.mapError (\_ -> Debug.crash "event list wasn't empty"))
                    `Task.andThen` (\logs ->
                      (API.getNumFrames newSession)
                      `Task.andThen` (\numFrames ->
                        API.getNodeStateSingle
                          newSession
                          (numFrames - 1)
                          [API.sgShape newSession |> .mainId]
                        |>  Task.mapError (Debug.crash << toString)
                        |>  Task.map (\valueSet ->
                              Response <|
                                StartWithHistoryResponse
                                  newSession
                                  (getMainVal newSession valueSet)
                                  numFrames
                                  logs)
                      )
                    )
                  )
                )
          in
            requestTask initTask { state | runningState <- Playing }

    Notification not ->
      case not of
        NewFrame newFrameNot ->
          let
            newMainVal =
              newFrameNot.subscribedNodeValues
                |> getMainVal state.session

            currentFrameIndex =
              state.numFrames

            mainNodeId =
              mainId state

            newExprLogs =
              updateLogs
                currentFrameIndex
                state.exprLogs
                newFrameNot.flaggedExprValues
                (always True)

            newNodeLogs =
              updateLogs
                currentFrameIndex
                state.nodeLogs
                newFrameNot.subscribedNodeValues
                (\id -> id /= mainNodeId)
          in
            done
              { state
                  | numFrames <- state.numFrames + 1
                  , exprLogs <- newExprLogs
                  , nodeLogs <- newNodeLogs
              }

        NoOpNot ->
          done state

    Response resp ->
      case resp of
        ScrubResponse mainVal ->
          requestTask
            (API.setMain state.session mainVal
              |> Task.map (always NoOp))
            state

        ForkResponse newSession frameIdx mainVal ->
          requestTask
            (API.setMain state.session mainVal
              |> Task.map (always NoOp))
            { state
                | session <- newSession
                , numFrames <- frameIdx + 1
                , exprLogs <- truncateLogs frameIdx state.exprLogs
                , nodeLogs <- truncateLogs frameIdx state.nodeLogs
            }

        SwapResponse newSession mainVal logs ->
          requestTask
            (API.setMain state.session mainVal
              |> Task.map (always NoOp))
            { state
                | session <- newSession
                , exprLogs <- Dict.fromList logs
            }

        StartWithHistoryResponse newSession mainVal numFrames logs ->
          requestTask
            (API.setMain state.session mainVal
              |> Task.map (always NoOp))
            { state
                | session <- newSession
                , numFrames <- numFrames
                , exprLogs <- Dict.fromList logs
            }

    NoOp ->
      done state


type CommandResponseMessage
  = SwapReplayError API.ReplayError
  -- clears the current mismatch error
  | SwapSuccessful
  | HistoryMismatchError
      { currentModuleName : API.ModuleName
      , historyModuleName : API.ModuleName
      }
  | ImportSessionSuccessful
  | NoOpResponse


commandResponseMailbox : () -> Signal.Mailbox CommandResponseMessage
commandResponseMailbox _ =
  mailbox


mailbox =
  Signal.mailbox NoOpResponse


getMainVal : API.DebugSession -> API.ValueSet -> API.JsElmValue
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


appendToLog : API.FrameIndex -> API.JsElmValue -> Maybe API.ValueLog -> Maybe API.ValueLog
appendToLog currentFrameIndex value maybeLog =
  let
    pair =
      (currentFrameIndex, value)

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
updateLogs currentFrameIndex logs updates idPred =
  List.foldl
    (\(tag, value) logs ->
      Dict.update tag (appendToLog currentFrameIndex value) logs)
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


curFrameIdx : Model -> API.FrameIndex
curFrameIdx model =
  case model.runningState of
    Paused idx ->
      idx

    _ ->
      model.numFrames - 1

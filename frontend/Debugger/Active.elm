module Debugger.Active where

import Dict exposing (Dict)
import Set exposing (Set)
import Debug
import Task exposing (Task)
import Time exposing (Time)

import Effects exposing (..)

import JsArray
import Debugger.RuntimeApi as API
import Debugger.Model as DM
import DataUtils exposing (..)


type alias Model =
  { window_ : DM.Window
  , session : DM.DebugSession
  , totalTimeLost : Time
  , runningState : RunningState
  , numFrames : Int
  , exprLogs : Dict DM.ExprTag DM.ValueLog
  , nodeLogs : Dict DM.NodeId DM.ValueLog
  , subscribedNodes : Set DM.NodeId
  }


initModel : DM.Window -> DM.DebugSession -> Model
initModel window_ session =
  { window_ = window_
  , session = session
  , totalTimeLost = 0
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
  | Paused DM.FrameIndex DM.ImmediateSessionRecord


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
  | ScrubTo DM.FrameIndex
  | Reset
  | Swap DM.CompiledElmModule
  | StartWithHistory DM.SessionRecord
  | NoOpCommand


type Notification
  = NewFrame DM.NewFrameNotification
  -- TODO: task update
  | NoOpNot


{-| Responses from the Runtime API that will be processed
within the Debugger service -}
type Response
  = ScrubResponse DM.FrameIndex DM.ImmediateSessionRecord DM.JsElmValue
  | ForkResponse DM.DebugSession DM.FrameIndex DM.JsElmValue
  | PausedResponse DM.FrameIndex DM.ImmediateSessionRecord
  | SwapResponse
      DM.DebugSession
      DM.JsElmValue
      (List (DM.ExprTag, DM.ValueLog))
      RunningState
  | StartWithHistoryResponse
      DM.DebugSession
      DM.JsElmValue
      Int
      (List (DM.ExprTag, DM.ValueLog))


update : Message -> Model -> (Model, Effects Message)
update msg state =
  case msg of
    Command cmd ->
      case cmd of
        Play ->
          case state.runningState of
            Paused pausedIdx record ->
              ( { state | runningState <- Playing }
              , playFrom state.window_ state.session record pausedIdx
                  |> task
              )

            Playing ->
              Debug.crash "already playing"

        Pause ->
          ( state
          , API.pause state.session
              |> Task.mapError (\_ -> Debug.crash "already in that state")
              |> Task.map (\record ->
                    Response <| PausedResponse (state.numFrames - 1) record)
              |> task
          )

        ScrubTo frameIdx ->
          let
            pauseAndGetState =
              (case state.runningState of
                Playing ->
                  API.pause state.session
                    |> Task.mapError (\_ -> Debug.crash "already in that state")

                Paused _ record ->
                  Task.succeed record)
              `Task.andThen` (\record ->
                API.getNodeStateSingle
                  state.session
                  frameIdx
                  [(API.getSgShape state.session).mainId]
                |> Task.mapError (Debug.crash << toString)
                |> Task.map (\valueSet ->
                      Response
                        (ScrubResponse
                          frameIdx
                          record
                          (getMainVal state.session valueSet)))
              )
          in
            ( state, pauseAndGetState |> task )

        Reset ->
          let
            getRecord =
              case state.runningState of
                Playing ->
                  (API.pause state.session
                    |> Task.mapError (\_ -> Debug.crash "already paused"))

                Paused _ record ->
                  record
                    |> API.splitRecord 0
                    |> fst
                    |> Task.succeed
          in
            ( { state | runningState <- Playing }
            , getRecord
              `Task.andThen` (\record ->
                playFrom state.window_ state.session record 0)
              |> task
            )

        Swap compiledMod ->
          let
            getRecord =
              case state.runningState of
                Playing ->
                  API.pause state.session
                    |> Task.mapError (\_ -> Debug.crash "already paused")

                Paused _ record ->
                  Task.succeed record

            swapTask =
              getRecord
              `Task.andThen` (\record ->
                (API.dispose state.session)
                `Task.andThen` (\_ ->
                  (API.instantiateModule state.window_ compiledMod)
                  `Task.andThen` (\newMod ->
                    (API.swap
                      state.window_
                      newMod
                      (API.getAddress state.session)
                      API.justMain
                      record.inputHistory
                      (Just (API.getSgShape state.session))
                      API.shapesEqual
                    |> Task.toResult)
                    `Task.andThen` (\swapRes ->
                      case swapRes of
                        Err replayError ->
                          Signal.send
                            (commandResponseMailbox ()).address
                            (SwapReplayError replayError)
                          |> Task.map (always NoOp)

                        Ok (newSession, exprLogs, nodeLogs) ->
                          (case state.runningState of
                            Playing ->
                              Task.succeed Playing

                            Paused idx _ ->
                              (API.pause newSession
                                |> Task.mapError (\_ -> Debug.crash "already paused")
                                |> Task.map (\newRecord ->
                                  Paused (JsArray.length newRecord.inputHistory) newRecord)))
                          `Task.andThen` (\newRunningState ->
                            let
                              mainVal =
                                nodeLogs
                                  |> List.filter (\(nodeId, log) ->
                                        nodeId == (API.getSgShape newSession).mainId)
                                  |> List.head
                                  |> getMaybe "no log for main"
                                  |> snd
                                  |> getLast
                                  |> getMaybe "no values in main log"
                                  |> snd
                            in
                              Task.succeed <|
                                Response <|
                                  SwapResponse
                                    newSession
                                    mainVal
                                    exprLogs
                                    newRunningState
                          )
                    )
                  )
                )
              )
          in
            ( state, swapTask |> task )

        StartWithHistory sessionRecord ->
          let
            currentModule =
              API.getModule state.session

            initTask =
              if currentModule.name /= sessionRecord.moduleName then
                let
                  error =
                    { currentModuleName = currentModule.name
                    , historyModuleName = sessionRecord.moduleName
                    }
                in
                  Signal.send
                    (commandResponseMailbox ()).address
                    (HistoryMismatchError error)
                  |> Task.map (always NoOp)
              else
                (API.dispose state.session)
                `Task.andThen` (\_ ->
                  (API.swap
                    state.window_
                    currentModule
                    (API.getAddress state.session)
                    API.justMain
                    sessionRecord.inputHistory
                    Nothing
                    (API.shapesEqual)
                  |> Task.mapError (\_ -> Debug.crash "replay error"))
                  `Task.andThen` (\(newSession, exprLogs, nodeLogs) ->
                    (API.subscribeToAll newSession API.justMain
                      |> Task.mapError (\_ -> Debug.crash "already subscribed"))
                    `Task.andThen` (\_ ->
                      let
                        numFrames =
                          JsArray.length sessionRecord.inputHistory - 1
                      in
                        API.getNodeStateSingle
                          newSession
                          numFrames
                          [API.getSgShape newSession |> .mainId]
                        |> Task.mapError (Debug.crash << toString)
                        |> Task.map (\valueSet ->
                              Response <|
                                StartWithHistoryResponse
                                  newSession
                                  (getMainVal newSession valueSet)
                                  numFrames
                                  exprLogs)
                    )
                  )
                )
          in
            ( { state | runningState <- Playing }
            , initTask |> task
            )

    Notification not ->
      case not of
        NewFrame newFrameNot ->
          case state.runningState of
            Paused _ _ ->
              Debug.crash "new frame while paused"

            Playing ->
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
                ( { state
                      | numFrames <- state.numFrames + 1
                      , exprLogs <- newExprLogs
                      , nodeLogs <- newNodeLogs
                  }
                , none
                )

        NoOpNot ->
          ( state, none )

    Response resp ->
      case resp of
        ScrubResponse frameIdx record mainVal ->
          ( { state | runningState <- Paused frameIdx record }
          , API.renderMain state.session mainVal
              |> Task.map (always NoOp)
              |> task
          )

        ForkResponse newSession frameIdx mainVal ->
          ( { state
                | session <- newSession
                , numFrames <- frameIdx + 1
                , exprLogs <- truncateLogs frameIdx state.exprLogs
                , nodeLogs <- truncateLogs frameIdx state.nodeLogs
            }
          , API.renderMain state.session mainVal
              |> Task.map (always NoOp)
              |> task
          )

        SwapResponse newSession mainVal logs newRunningState ->
          ( { state
                | session <- newSession
                , runningState <- newRunningState
                , exprLogs <- Dict.fromList logs
            }
          , API.renderMain state.session mainVal
              |> Task.map (always NoOp)
              |> task
          )

        StartWithHistoryResponse newSession mainVal numFrames logs ->
          ( { state
                | session <- newSession
                , numFrames <- numFrames
                , exprLogs <- Dict.fromList logs
            }
          , API.renderMain state.session mainVal
              |> Task.map (always NoOp)
              |> task
          )

        PausedResponse frameIdx record ->
          ( { state | runningState <- Paused frameIdx record }
          , none
          )

    NoOp ->
      ( state, none )


-- should this be in RuntimeApi?
playFrom : DM.Window
        -> DM.DebugSession
        -> DM.ImmediateSessionRecord
        -> DM.FrameIndex
        -> Task x Message
playFrom window_ session record frameIdx =
  (API.dispose session)
  `Task.andThen` (\_ ->
    (let
      (beforeRecord, _) =
        API.splitRecord frameIdx record
    in
      API.play window_ beforeRecord (API.getAddress session) API.justMain
      |> Task.map (\(newSession, valueSet) ->
          Response <|
            ForkResponse
              newSession
              frameIdx
              (getMainVal newSession valueSet))
    )
  )


{-| Responses that need to go back to the UI -}
type CommandResponseMessage
  = SwapReplayError DM.ReplayError
  -- clears the current mismatch error
  | SwapSuccessful
  | HistoryMismatchError
      { currentModuleName : DM.ModuleName
      , historyModuleName : DM.ModuleName
      }
  | ImportSessionSuccessful
  | NoOpResponse


commandResponseMailbox : () -> Signal.Mailbox CommandResponseMessage
commandResponseMailbox _ =
  mailbox


mailbox =
  Signal.mailbox NoOpResponse


getMainVal : DM.DebugSession -> DM.ValueSet -> DM.JsElmValue
getMainVal session values =
  let
    mainId =
      (API.getSgShape session).mainId

  in
    values
      |> List.filter (\(id, val) -> id == mainId)
      |> List.head
      |> getMaybe "no value with main id"
      |> snd


appendToLog : DM.FrameIndex -> DM.JsElmValue -> Maybe DM.ValueLog -> Maybe DM.ValueLog
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


updateLogs : DM.FrameIndex
          -> Dict comparable DM.ValueLog
          -> List (comparable, DM.JsElmValue)
          -> (comparable -> Bool)
          -> Dict comparable DM.ValueLog
updateLogs currentFrameIndex logs updates idPred =
  List.foldl
    (\(tag, value) logs ->
      Dict.update tag (appendToLog currentFrameIndex value) logs)
    logs
    (List.filter (fst >> idPred) updates)


truncateLogs : DM.FrameIndex -> Dict comparable DM.ValueLog -> Dict comparable DM.ValueLog
truncateLogs frameIdx logs =
  logs
    |> Dict.map (\_ log -> truncateLog frameIdx log)
    |> Dict.filter (\_ log -> not (List.isEmpty log))


truncateLog : DM.FrameIndex -> DM.ValueLog -> DM.ValueLog
truncateLog frameIdx log =
  List.filter (\(idx, val) -> idx <= frameIdx) log


isPlaying : Model -> Bool
isPlaying model =
  case model.runningState of
    Playing ->
      True

    _ ->
      False


mainId : Model -> DM.NodeId
mainId model =
  (API.getSgShape model.session).mainId


curFrameIdx : Model -> DM.FrameIndex
curFrameIdx model =
  case model.runningState of
    Paused idx _ ->
      idx

    _ ->
      model.numFrames - 1

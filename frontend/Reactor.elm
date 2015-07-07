module Reactor where

import Signal
import Time
import Array as A
import Dict as D
import Task as T
import Maybe as M
import Html exposing (..)
import Json.Encode as JsEncode
import Debug

import Button
import Overlay
import Model exposing (..)


main : Signal Html
main =
  Signal.map (Overlay.view uiActionsMailbox.address) (Signal.map fst stateAndTasks)


stateAndTasks : Signal (Model, Maybe (T.Task String ()))
stateAndTasks =
  let
    initModel =
      initState initSnapshot timeStarted
  in
    Signal.foldp update (initModel, Just <| T.succeed ()) allActions


uiActionsMailbox : Signal.Mailbox Action
uiActionsMailbox =
  Signal.mailbox NoOp


allActions : Signal (Time.Time, Action)
allActions =
  Time.timestamp <|
    Signal.mergeMany
      [ uiActionsMailbox.signal
      , Signal.map NewEvent events
      , Signal.map NewSnapshot snapshots
      ]

-- don't forget about applying the watch updates that come with events!
-- TODO: second param should be just model
-- foldp shouldn't really be holding on to tasks...
update : (Time.Time, Action) -> (Model, Maybe (T.Task String ())) -> (Model, Maybe (T.Task String ()))
update (now, action) (state, _) =
  case action of
    Restart ->
      let
        (newRunningState, maybeDelay) =
          case state.runningState of
            Running _ ->
              let
                delay = now - state.timeStarted
              in 
                ( Running delay
                , Just delay
                )

            Paused time _ ->
              ( Paused time 0
              , Nothing
              )

        delayTask =
          case maybeDelay of
            Just delay ->
              Signal.send delayUpdateMailbox.address delay

            Nothing ->
              T.succeed ()

        initSnapshot =
          state.snapshots
            |> A.get 0
            |> getOrCrash "initial snapshot gone"

        initSnapshotTask =
          Signal.send setToSnapshotMailbox.address initSnapshot.signalGraph
      in
        ( { state
              | runningState <- newRunningState
              , events <- A.empty
              , snapshots <- state.snapshots |> A.slice 0 1
              , currentWatches <- D.empty
          }
        , Just <| delayTask `T.andThen` (always initSnapshotTask)
        )

    PlayPause willBePlaying ->
      let
        (newState, maybeDelay) =
          pausePlay state willBePlaying now

        maybeDelayTask =
          maybeDelay `M.andThen`
            \delay -> Just <| Signal.send delayUpdateMailbox.address delay
      in
        (newState, maybeDelayTask)

    ScrubPosition newFrameIdx ->
      let
        newRunningState =
          Paused now newFrameIdx

        curSnapshot =
          closestSnapshotBefore (curFrameIdx state)

        newSnapshot =
          closestSnapshotBefore newFrameIdx

        snapshotNeeded =
          curSnapshot /= newSnapshot || newFrameIdx < (curFrameIdx state)

        snapshot =
          if snapshotNeeded then
            state |> snapshotAtIdx newSnapshot |> Just
          else
            Nothing

        snapshotTask =
          case snapshot of
            Just ss ->
              Signal.send setToSnapshotMailbox.address ss.signalGraph

            Nothing ->
              T.succeed ()

        events =
          if snapshotNeeded then
            state.events
              |> A.slice (newSnapshot * eventsPerSnapshot) newFrameIdx
          else
            state.events
              |> A.slice (curFrameIdx state) newFrameIdx

        eventsTask =
          Signal.send processEventsMailbox.address events

        baseWatches =
          case snapshot of
            Just ss ->
              ss.watches
            Nothing ->
              state.currentWatches

        watches =
          A.foldl applyWatchUpdate baseWatches (A.map .watchUpdate events)
      in
        ( { state
              | runningState <- newRunningState
              , currentWatches <- watches
          }
        , Just <| snapshotTask `T.andThen` (always eventsTask)
        )

    NewEvent evt ->
      case state.runningState of
        Running _ ->
          let
            task =
              if (curFrameIdx state) % eventsPerSnapshot == 0 then
                Just <| Signal.send captureSnapshotMailbox.address ()
              else
                Nothing

            newWatches =
              applyWatchUpdate evt.watchUpdate state.currentWatches
          in
            ( { state
                  | events <- A.push evt state.events
                  , currentWatches <- newWatches
              }
            , task
            )
        _ ->
          Debug.crash "new event while paused"

    NewSnapshot sgSnapshot ->
      case state.runningState of
        Running _ ->
          let
            snapshot =
              { signalGraph = sgSnapshot
              , watches = state.currentWatches
              }
          in
            ( { state |
                  snapshots <- A.push snapshot state.snapshots
              }
            , Nothing
            )
        _ ->
          Debug.crash "new snapshot while paused"

    SidebarVisible visible ->
      ( { state |
            sidebarVisible <- visible
        }
      , Nothing
      )

    PermitSwap permit ->
      ( { state |
            permitSwap <- permit
        }
      , Nothing
      )

    RestartButtonAction buttonAct ->
      ( { state |
            restartButtonState <- Button.update buttonAct state.restartButtonState
        }
      , Nothing
      )

    PlayPauseButtonAction buttonAct ->
      ( { state |
            playPauseButtonState <- Button.update buttonAct state.restartButtonState
        }
      , Nothing
      )

    NoOp ->
      ( state, Nothing )


-- incoming

port initSnapshot : SGSnapshot

port timeStarted : Time.Time

port events : Signal Event

port snapshots : Signal SGSnapshot

-- outgoing

-- does this method guarantee that we'll get a response within
-- the same frame?
captureSnapshotMailbox : Signal.Mailbox ()
captureSnapshotMailbox =
  Signal.mailbox ()

port captureSnapshot : Signal ()
port captureSnapshot =
  captureSnapshotMailbox.signal


setToSnapshotMailbox : Signal.Mailbox SGSnapshot
setToSnapshotMailbox =
  Signal.mailbox JsEncode.null

port setToSnapshot : Signal SGSnapshot
port setToSnapshot =
  setToSnapshotMailbox.signal


processEventsMailbox : Signal.Mailbox (A.Array Event)
processEventsMailbox =
  Signal.mailbox A.empty

port processEvents : Signal (A.Array Event)
port processEvents =
  processEventsMailbox.signal


delayUpdateMailbox : Signal.Mailbox Time.Time
delayUpdateMailbox =
  Signal.mailbox 0

port delayUpdates : Signal Time.Time
port delayUpdates =
  delayUpdateMailbox.signal


port permitSwap : Signal Bool
port permitSwap =
  let
    fun act =
      case act of
        PermitSwap permit ->
          Just permit

        _ ->
          Nothing
  in
    Signal.filterMap fun True uiActionsMailbox.signal


port tasks : Signal (T.Task String ())
port tasks =
  Signal.filterMap snd (T.succeed ()) stateAndTasks

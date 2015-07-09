module Debugger where

import Signal
import Time
import Array as A
import Dict as D
import Task as T
import Maybe as M
import Html exposing (..)
import Json.Encode as JsEncode
import Debug
import StartApp

import Button
import Overlay
import Model exposing (..)


main : Signal Html
main =
  fst viewAndTasks


port tasks : Signal (T.Task String ())
port tasks =
  snd viewAndTasks


viewAndTasks : (Signal Html, Signal (T.Task String ()))
viewAndTasks =
  StartApp.start
    { initialState = Model.initState
    , view = Overlay.view
    , update = update
    }
    externalActions


externalActions : Signal Action
externalActions =
  Signal.mergeMany
    [ Signal.map NewEvent events
    , Signal.map NewSnapshot snapshots
    ]

-- don't forget about applying the watch updates that come with events!
-- TODO: second param should be just model
-- foldp shouldn't really be holding on to tasks...
update : StartApp.LoopbackFun String Action
      -> Time.Time
      -> Action
      -> Model
      -> (Model, List (T.Task String ()))
update loopback now action state =
  case state.attachmentState of
    Uninitialized ->
      case action of
        Attach initSgSnapshot ->
          let
            attachmentState =
              Connected
                { snapshots = A.fromList [makeInitSnapshot initSgSnapshot]
                , currentWatches = emptyWatchSnapshot
                , timeStarted = now
                }
          in
            ( { state | attachmentState <- attachmentState }
            , []
            )

        _ ->
          Debug.crash <|
            "unexpected action in Uninitialized state: " ++ (toString action)

    Swapping timeStarted ->
      case action of
        Attach initSgSnapshot ->
          let
            attachmentState =
              Connected
                { snapshots = A.fromList [makeInitSnapshot initSgSnapshot]
                , currentWatches = emptyWatchSnapshot
                , timeStarted = timeStarted
                }
          in
            ( { state | attachmentState <- attachmentState }
            , []
            )

        _ ->
          Debug.crash <|
            "unexpected action in Swapping state: " ++ (toString action)

    Connected connectedAttrs ->
      case Debug.log "action" action of
        Restart ->
          let
            (newRunningState, maybeDelay) =
              case state.runningState of
                Running _ ->
                  let
                    delay = now - connectedAttrs.timeStarted
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
              connectedAttrs.snapshots
                |> A.get 0
                |> getOrCrash "initial snapshot gone!!"

            initSnapshotTask =
              Signal.send setToSnapshotMailbox.address initSnapshot.signalGraph
          in
            ( { state
                  | runningState <- newRunningState
                  , events <- A.empty
                  , attachmentState <-
                      connectedAttrs
                        |> updateSnapshots (A.slice 0 1)
                        |> (\(Connected attrs) -> attrs)
                        |> updateCurrentWatches (always D.empty)
              }
            , [delayTask, initSnapshotTask]
            )

        PlayPause willBePlaying ->
          let
            (newState, maybeDelay) =
              case state.runningState of
                Running delay ->
                  if willBePlaying then
                    Debug.crash "already playing"
                  else
                    ( { state |
                          runningState <- Paused now (numFrames state)
                      }
                    , Nothing
                    )

                Paused pausedAtTime idx ->
                  if willBePlaying then
                    let
                      lastEvtTime =
                        if curFrameIdx state == idx then
                          pausedAtTime
                        else if idx == 0 then
                          connectedAttrs.timeStarted
                        else
                          state.events
                            |> A.get (idx - 1)
                            |> getOrCrash "no event at index"
                            |> .time

                      delay =
                        now - lastEvtTime
                    in
                      -- discard events and snapshots after where we're unpausing
                      ( { state 
                            | runningState <- Running delay
                            , events <- state.events |> A.slice 0 idx
                            , attachmentState <-
                                connectedAttrs
                                  |> updateSnapshots (A.slice 0 (closestSnapshotBefore idx))
                        }
                      , Just delay
                      )

                  else
                    Debug.crash "already paused"

            maybeDelayTask =
              maybeDelay `M.andThen`
                (\delay -> Just <| Signal.send delayUpdateMailbox.address delay)
          in
            (state, maybeDelayTask |> maybeToList)

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
                connectedAttrs |> snapshotAtIdx newSnapshot |> Just
              else
                Nothing

            snapshotTask =
              case snapshot of
                Just ss ->
                  Signal.send setToSnapshotMailbox.address ss.signalGraph

                Nothing ->
                  T.succeed ()

            events =
              if (curFrameIdx state) == newFrameIdx then
                Debug.log "empty" A.empty
              else if snapshotNeeded then
                let
                  a = Debug.log "SNAPSHOT" (newFrameIdx, A.length state.events, newSnapshot * eventsPerSnapshot, newFrameIdx)
                in
                  state.events
                    |> A.slice (newSnapshot * eventsPerSnapshot) newFrameIdx
              else
                let
                  a = Debug.log "NO SNAPSHOT" (newFrameIdx, A.length state.events, (curFrameIdx state), newFrameIdx)
                in
                  state.events
                    |> A.slice (curFrameIdx state) newFrameIdx

            eventsTask =
              Signal.send processEventsMailbox.address events

            baseWatches =
              case snapshot of
                Just ss ->
                  ss.watches
                Nothing ->
                  connectedAttrs.currentWatches

            newWatches =
              A.foldl applyWatchUpdate baseWatches (A.map .watchUpdate events)
          in
            ( { state
                  | runningState <- newRunningState
                  , attachmentState <-
                      connectedAttrs
                        |> updateCurrentWatches (always newWatches)
              }
            , [snapshotTask, eventsTask]
            )

        NewEvent evt ->
          case state.runningState of
            Running _ ->
              let
                task =
                  if (curFrameIdx state) % eventsPerSnapshot == 0 then
                    [Signal.send captureSnapshotMailbox.address ()]
                  else
                    []

                newWatches =
                  applyWatchUpdate evt.watchUpdate connectedAttrs.currentWatches
              in
                ( { state
                      | events <- A.push evt state.events
                      , attachmentState <-
                          connectedAttrs
                            |> updateCurrentWatches (always newWatches)
                  }
                , task
                )
            _ ->
              --Debug.crash "new event while paused"
              (state, [])

        NewSnapshot sgSnapshot ->
          case state.runningState of
            Running _ ->
              let
                snapshot =
                  { signalGraph = sgSnapshot
                  , watches = connectedAttrs.currentWatches
                  }
              in
                ( { state |
                      attachmentState <-
                        connectedAttrs |> updateSnapshots (A.push snapshot)
                  }
                , []
                )
            _ ->
              Debug.crash "new snapshot while paused"

        PermitSwap permit ->
          ( { state |
                permitSwap <- permit
            }
          , []
          )


-- incoming

port initSnapshot : SGSnapshot

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

port delayUpdate : Signal Time.Time
port delayUpdate =
  delayUpdateMailbox.signal


port permitSwap : Signal Bool
port permitSwap =
  --let
  --  fun act =
  --    case act of
  --      PermitSwap permit ->
  --        Just permit

  --      _ ->
  --        Nothing
  --in
  --  Signal.filterMap fun True uiActionsMailbox.signal
  Signal.constant True -- TODO !!

maybeToList : Maybe a -> List a
maybeToList maybe =
  case maybe of
    Just x ->
      [x]

    Nothing ->
      []

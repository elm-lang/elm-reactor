module Reactor where

import Dict as D
import Array as A
import Time
import Task as T
import Signal
import Json.Encode as JsEncode
import Maybe as M
import Debug

import Button

type alias DebugState =
    { runningState : RunningState
    -- , totalTimeLost : ?
    , events : A.Array Event
    , snapshots : A.Array Snapshot
    , timeStarted : Time.Time
    -- async callbacks?
    -- traces : ?
    , permitSwap : Bool
    -- TODO: move into sidebar model?
    , sidebarVisible : Bool
    , restartButtonState : Button.Model
    , playPauseButtonState : Button.Model
    }

initState : SGSnapshot -> Time.Time -> DebugState
initState snapshot timeStarted =
  { runningState = Running 0
  -- , totalTimeLost = ?
  , events = A.empty
  , snapshots = A.fromList [emptySnapshot]
  , timeStarted = timeStarted
  -- async callbacks?
  -- traces = ?
  , permitSwap = True
  -- TODO: move into sidebar model?
  , sidebarVisible = True
  , restartButtonState = Button.Up
  , playPauseButtonState = Button.Up
  }

type Action
    -- UI actions
    = Restart Time.Time
    | Pause Bool Time.Time -- will be playing
    | ScrubPosition FrameIndex Time.Time
    | SidebarVisible Bool
    | PermitSwap Bool
    | RestartButtonAction Button.Action
    | PlayPauseButtonAction Button.Action
    -- events from RTS
    | NewEvent Event
    | NewSnapshot Snapshot
    | NoOp

type RunningState
    = Running Time.Time -- delay
    | Paused Time.Time FrameIndex -- paused at time & idx

type alias FrameIndex = Int

type alias SnapshotIndex = Int

-- EVENTS

-- TODO: add trace updates

type alias Event =
    { id : SGNodeId
    , value : ElmValue
    , time : Time.Time
    , watchUpdates : List (String, String)
    }

type alias SGNodeId = Int

type alias ElmValue =
    JsEncode.Value

-- SNAPSHOTS

type alias Snapshot =
    { signalGraph : SGSnapshot
    , watches : WatchSnapshot
    }

-- signal graph

type alias SGSnapshot =
    JsEncode.Value -- no need to decode these from JS

emptySnapshot : Snapshot
emptySnapshot =
  { signalGraph = JsEncode.null -- TODO: array?
  , watches = D.empty
  }

-- WATCHES

type alias WatchSnapshot =
    D.Dict WatchId ElmValue

type alias WatchId = String

-- Update

-- don't forget about applying the watch updates that come with events!
update : Action -> DebugState -> (DebugState, Maybe (T.Task x ()))
update action state =
  case action of
    Restart now ->
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
          }
        , Just <| delayTask `T.andThen` (always initSnapshotTask)
        )

    Pause willBePlaying now ->
      let
        (newState, maybeDelay) =
          pausePlay state willBePlaying now

        maybeDelayTask =
          maybeDelay `M.andThen`
            \delay -> Just <| Signal.send delayUpdateMailbox.address delay
      in
        (newState, maybeDelayTask)

    ScrubPosition newFrameIdx time ->
      let
        newRunningState =
          Paused time newFrameIdx

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
      in
        ( { state |
              runningState <- newRunningState
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
          in
            ( { state |
                  events <- A.push evt state.events
              }
            , Nothing -- TODO: send a "take snapshot" command? XXX
            )
        _ ->
          Debug.crash "new event while paused"

    NewSnapshot snapshot ->
      case state.runningState of
        Running _ ->
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


curFrameIdx : DebugState -> FrameIndex
curFrameIdx state =
  case state.runningState of
    Running _ ->
      numFrames state

    Paused _ idx ->
      idx


numFrames : DebugState -> Int
numFrames state =
  A.length state.events + 1


eventsPerSnapshot : Int
eventsPerSnapshot =
  100


closestSnapshotBefore : FrameIndex -> SnapshotIndex
closestSnapshotBefore idx =
  floor <| (toFloat idx) / (toFloat eventsPerSnapshot)


snapshotAtIdx : SnapshotIndex -> DebugState -> Snapshot
snapshotAtIdx idx state =
  state.snapshots
    |> A.get idx
    |> getOrCrash ("no snapshot at index " ++ (toString idx))


pausePlay : DebugState -> Bool -> Time.Time -> (DebugState, Maybe Time.Time)
pausePlay state willBePlaying now =
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
              state.timeStarted
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
                , snapshots <- state.snapshots |> A.slice 0 (closestSnapshotBefore idx)
            }
          , Just delay
          )

      else
        Debug.crash "already paused"


getOrCrash : String -> Maybe a -> a
getOrCrash msg maybe =
  case maybe of
    Just x ->
      x

    Nothing ->
      Debug.crash msg


-- incoming

port events : Signal Event

port snapshots : Signal SGSnapshot

port watchUpdates : Signal (String, String)

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

module Model where

import Dict as D
import Array as A
import Time
import Task as T
import Signal
import Json.Encode as JsEncode
import Maybe as M
import Debug

import Button

type alias Model =
    { runningState : RunningState
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

initState : SGSnapshot -> Time.Time -> Model
initState snapshot timeStarted =
  { runningState = Running 0
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
    | PlayPause Bool Time.Time -- will be playing
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


isPaused : Model -> Bool
isPaused model =
  case model.runningState of
    Running _ ->
      False

    Paused _ _ ->
      True


curFrameIdx : Model -> FrameIndex
curFrameIdx state =
  case state.runningState of
    Running _ ->
      numFrames state

    Paused _ idx ->
      idx


numFrames : Model -> Int
numFrames state =
  A.length state.events + 1


eventsPerSnapshot : Int
eventsPerSnapshot =
  100


closestSnapshotBefore : FrameIndex -> SnapshotIndex
closestSnapshotBefore idx =
  floor <| (toFloat idx) / (toFloat eventsPerSnapshot)


snapshotAtIdx : SnapshotIndex -> Model -> Snapshot
snapshotAtIdx idx state =
  state.snapshots
    |> A.get idx
    |> getOrCrash ("no snapshot at index " ++ (toString idx))


pausePlay : Model -> Bool -> Time.Time -> (Model, Maybe Time.Time)
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

module Model where

import Dict as D
import Array as A
import Time
import Task as T
import Signal
import Json.Encode as JsEncode
import Maybe as M
import Debug

import OverlayModel
import Button

type alias Model =
    { runningState : RunningState
    , events : A.Array Event
    , attachmentState : AttachmentState
    -- async callbacks?
    , permitSwap : Bool
    , overlayModel : OverlayModel.Model
    }

type RunningState
    = Running Time.Time -- delay
    | Paused Time.Time FrameIndex -- paused at time & idx

{-| Start uninitialized.
From there, alternate between swapping and connected.
-}
type AttachmentState
  = Uninitialized
  | Swapping Time.Time
  | Connected ConnectedAttrs

type alias ConnectedAttrs =
  { snapshots : A.Array Snapshot
  , currentWatches : WatchSnapshot
  , timeStarted : Time.Time
  -- traces : ?
  }

initState : Model
initState =
  { runningState = Running 0
  , events = A.empty
  , attachmentState = Uninitialized
  -- async callbacks?
  , permitSwap = True
  -- TODO: move into sidebar model?
  , overlayModel = OverlayModel.initState
  }

type Action
    -- actions from UI
    = Restart
    | PlayPause Bool -- will be playing
    | ScrubPosition FrameIndex
    | PermitSwap Bool
    | OverlayAction OverlayModel.Action
    -- events from RTS
    | NewEvent Event
    | NewSnapshot SGSnapshot
    | Attach SGSnapshot
    | StartSwap

type alias FrameIndex = Int

type alias SnapshotIndex = Int

-- EVENTS

-- TODO: add trace updates

type alias Event =
    { id : SGNodeId
    , value : ElmValue
    , time : Time.Time
    , watchUpdate : WatchUpdate
    }

type alias SGNodeId = Int

type alias ElmValue =
    JsEncode.Value

-- SNAPSHOTS

type alias Snapshot =
    { signalGraph : SGSnapshot
    , watches : WatchSnapshot
    }

makeInitSnapshot : SGSnapshot -> Snapshot
makeInitSnapshot sgSnap =
  { signalGraph = sgSnap
  , watches = emptyWatchSnapshot
  }

-- signal graph

type alias SGSnapshot =
    JsEncode.Value -- no need to decode these from JS

-- WATCHES

type alias WatchUpdate =
    List (WatchId, String)

type alias WatchSnapshot =
    D.Dict WatchId String

type alias WatchId = String


emptyWatchSnapshot : WatchSnapshot
emptyWatchSnapshot =
  D.empty


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


snapshotAtIdx : SnapshotIndex -> ConnectedAttrs -> Snapshot
snapshotAtIdx idx connected =
  connected.snapshots
    |> A.get idx
    |> getOrCrash ("no snapshot at index " ++ (toString idx))


applyWatchUpdate : WatchUpdate -> WatchSnapshot -> WatchSnapshot
applyWatchUpdate update snapshot =
  D.union (D.fromList update) snapshot

-- these would be unnecessary if we had nested record updates

updateSnapshots : (A.Array Snapshot -> A.Array Snapshot)
               -> ConnectedAttrs
               -> AttachmentState
updateSnapshots updateFn attrs =
  Connected
    { attrs | snapshots <- updateFn attrs.snapshots }


updateCurrentWatches : (WatchSnapshot -> WatchSnapshot)
                    -> ConnectedAttrs
                    -> AttachmentState
updateCurrentWatches updateFn attrs =
  Connected
    { attrs | currentWatches <- updateFn attrs.currentWatches }

-- Util

getOrCrash : String -> Maybe a -> a
getOrCrash msg maybe =
  case maybe of
    Just x ->
      x

    Nothing ->
      Debug.crash msg

module Reactor where

import Dict as D
import Array as A
import Time
import Task as T
import Signal
import Json.Encode as JsEncode

import Button

type alias DebugState =
    { runningState : RunningState
    -- , totalTimeLost : ?
    , events : A.Array Event
    , snapshots : A.Array Snapshot
    , timeStarted : Time.Time
    -- async callbacks?
    -- traces : ?
    , permitSwaps : Bool
    , sidebarVisible : Bool
    }

type Action
    -- UI actions
    = Restart
    | Pause Bool
    | ScrubPosition FrameIndex
    | SidebarVisible Bool
    | PermitSwap Bool
    | RestartButtonAction Button.Action
    | PlayPauseButtonAction Button.Action
    -- events from RTS
    | NewEvent Event
    | NewSnapshot Snapshot
    | UpdateWatch String String
    | NoOp

type RunningState
    = Running Time.Time
    | Paused Time.Time FrameIndex

type alias FrameIndex = Int

-- EVENTS

-- TODO: add trace updates

type alias Event =
    { id : SGNodeId
    , value : ElmValue
    , time : Time.Time
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

update : Action -> DebugState -> DebugState
update action state =
  state -- TODO

-- incoming

port events : Signal Event

port snapshots : Signal SGSnapshot

port watchUpdates : Signal (String, String)

-- outgoing

captureSnapshotMailbox : Signal.Mailbox (T.Task String ())
captureSnapshotMailbox =
  Signal.mailbox <| T.succeed ()

port captureSnapshot : Signal (T.Task String ())
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

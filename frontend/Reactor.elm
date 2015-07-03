module Reactor where

import Dict as D
import Array as A
import Time
import Task as T
import Signal

type alias DebugState =
    { runningState : RunningState
    -- , totalTimeLost : ?
    , events : A.Array Event
    , snapshots : A.Array Snapshot
    , timeStarted : Time
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
    = Running Time
    | Paused Time FrameIndex

type alias FrameIndex = Int

-- EVENTS

-- TODO: add trace updates

type alias Event =
    { id : SGNodeId
    , value : ElmValue
    , time : Time
    , watchUpdates : Dict WatchId ElmValue
    }

-- SNAPSHOTS

type alias Snapshot =
    { signalGraph : SGSnapshot
    , watches : WatchSnapshot
    }

-- signal graph

type alias SGSnapshot = Dict SGNodeId SignalValue

type alias SGNodeId = Int

type alias SignalValue =
    { id : Int
    , value : ElmValue
    }

emptySnapshot : Snapshot
emptySnapshot =
  { signalGraph = D.emtpy
  , watches = D.empty
  }

-- WATCHES

type alias WatchSnapshot = Dict WatchId ElmValue

type alias WatchId = String

-- Update

update : Action -> DebugState -> DebugState
update action state =
  ...

-- incoming

port events : Signal Event

port snapshots : Signal SGSnapshot

port watchUpdates : Signal (String, String)

-- outgoing

captureSnapshotMailbox : Signal.Mailbox (T.Task String ())
captureSnapshotMailbox =
  Signal.mailbox T.succeed

port captureSnapshot : Signal (T.Task String ())
port captureSnapshot =
  captureSnapshotMailbox.signal


setToSnapshotMailbox : Signal.Mailbox Snapshot
setToSnapshotMailbox =
  Signal.mailbox emptySnapshot

port setToSnapshot : Signal Snapshot
port setToSnapshot =
  setToSnapshotMailbox.signal


processEventsMailbox : Signal.Mailbox (A.Array Event)
processEventsMailbox =
  Signal.mailbox A.empty

port processEvents : Signal (A.Array Event)
port processEvents =
  processEventsMailbox.signal

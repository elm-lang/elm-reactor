module Reactor where

type alias DebugState =
    { runningState : RunningState
    -- , totalTimeLost : ?
    , events : Array Event
    , snapshots = Array Snapshot
    -- async callbacks?
    -- traces : ?
    , permitSwaps : Bool
    }

type RunningState
    = Running
    | Paused Time FrameIndex -- ?

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

-- WATCHES

type alias WatchSnapshot = Dict WatchId ElmValue

type alias WatchId = String

-- incoming

port events : Signal Event

-- outgoing

port captureSnapshot : Signal (Task e Snapshot)
port captureSnapshot = ...

port setToSnapshot : Signal Snapshot
port setToSnapshot = ...

port processEvents : Signal (Array Event)
port processEvents = ...

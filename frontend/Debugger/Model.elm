module Debugger.Model where

import Json.Encode as JsEnc
import Time exposing (Time)
import Dict exposing (Dict)

import JsArray exposing (..)


type alias FrameIndex =
  Int


type alias JsElmValue =
  JsEnc.Value


type DebugSession
  = DebugSession


type alias NodeId =
  Int


type StateError
  = IsDisposed
  | IsPlaying -- TODO: using 2 modules should get rid of this
  | EventIndexOutOfRange FrameInterval


type alias Event =
  { value : JsElmValue
  , nodeId : NodeId
  , time : Time
  }


type Snapshot =
  Snapshot


emptyEvent =
  { value = JsEnc.null
  , nodeId = 0
  , time = 0
  }


type alias NewFrameNotification =
  { event : Event
  , flaggedExprValues : List (ExprTag, JsElmValue)
  , subscribedNodeValues : ValueSet
  }


emptyNotification =
  { event = emptyEvent
  , flaggedExprValues = []
  , subscribedNodeValues = []
  }

-- for time being, these are only set by Debug.log
type alias ExprTag =
  String


{-| Interpreted as inclusive -}
type alias FrameInterval =
  { start : FrameIndex
  , end : FrameIndex
  }


type alias ValueLog =
  List (FrameIndex, JsElmValue)


type alias ValueSet =
  List (NodeId, JsElmValue)


type alias CompiledElmModule =
  { name : ModuleName
  , code : String -- javascript
  }


type alias ElmModule =
  { name : ModuleName
  , modul : JsEnc.Value
  }


type alias ModuleName =
  String


type alias SGShape =
  { nodes : Dict NodeId NodeInfo
  , mainId : NodeId
  }


type alias NodeInfo =
  { name : String
  , nodeType : NodeType
  , kids : List NodeId
  }


type NodeType
  = InputPort
  | CoreLibInput
  | Mailbox
  | InternalNode
  | OutputPort
  | Main


type alias SessionRecord =
  { moduleName : ModuleName
  , inputHistory : InputHistory
  }


type alias InputHistory =
  JsArray Event


-- TODO: better name
type alias ImmediateSessionRecord =
  { sgShape : SGShape
  , modul : ElmModule
  , snapshots : JsArray Snapshot
  , inputHistory : InputHistory
  , delay : Time
  , startedAt : Time
  , pausedAt : Time
  }


type alias ReplayError =
  { oldShape : SGShape
  , newShape : SGShape
  }

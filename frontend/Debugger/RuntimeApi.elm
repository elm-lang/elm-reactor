module Debugger.RuntimeApi where

import Task exposing (Task)
import Dict
import Json.Encode as JsEnc
import Time

import Native.Debugger.RuntimeApi


type alias FrameIndex =
  Int


type alias JsElmValue =
  JsEnc.Value


type DebugSession
  = DebugSession -- opaque


type alias NodeId =
  Int


type alias Event =
  { value : JsElmValue
  , nodeId : NodeId
  , time : Time.Time
  }


emptyEvent =
  { value = JsEnc.null
  , nodeId = 0
  , time = 0
  }


sgShape : DebugSession -> SGShape
sgShape =
  Native.Debugger.RuntimeApi.sgShape


numFrames : DebugSession -> Int
numFrames =
  Native.Debugger.RuntimeApi.numFrames


-- can decode

-- new frame
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


-- QUERIES

{-| We do this with *lists* of nodes or exprs
a/o just one at a time, because simulating the module forward in the
specified time interval and gathering the all SG or expr values we're
interested in at each step is better than simulating it once for each
point of interest. -}
getNodeState : DebugSession -> FrameInterval -> List NodeId -> Task String (List (NodeId, ValueLog))
getNodeState =
  Native.Debugger.RuntimeApi.getNodeState


getInputHistory : DebugSession -> InputHistory
getInputHistory =
  Native.Debugger.RuntimeApi.getHistory


emptyInputHistory : InputHistory
emptyInputHistory =
  Native.Debugger.RuntimeApi.emptyInputHistory


{-|
- Forgets all frames after given frame index
- Returns values of subscribed nodes at given frame index
- Module plays from given frame index
-}
forkFrom : DebugSession -> FrameIndex -> Task String ValueSet
forkFrom =
  Native.Debugger.RuntimeApi.forkFrom


{-| Interpreted as inclusive -}
type alias FrameInterval =
  { start : FrameIndex
  , end : FrameIndex
  }


type alias ValueLog =
  List (FrameIndex, JsElmValue)


type alias ValueSet =
  List (NodeId, JsElmValue)


-- COMMANDS

{-| Swap in new module. Starts off playing.
Subscribes to the list of nodes returned by the given function (3rd arg),
and returns their initial values. -}
initialize : ElmModule
          -> InputHistory
          -> Signal.Address NewFrameNotification
          -> (SGShape -> List NodeId)
          -> Task SwapError (DebugSession, ValueSet)
initialize =
  Native.Debugger.RuntimeApi.initialize


-- how is the previous session killed?

type alias ElmModule =
  JsEnc.Value


evalModule : CompiledElmModule -> ElmModule
evalModule =
  Native.Debugger.RuntimeApi.evalModule


type alias CompiledElmModule =
  { name : String
  , code : String -- javascript
  }


-- opaque
-- must start from time 0
-- TODO: if this is opaque, how are we gonna serialize it for download?
-- could just JSON-ize it on the JS side...
type InputHistory =
  InputHistory


type alias SubscriptionSet =
  List NodeId


type alias SGShape =
  { nodes : Dict.Dict NodeId NodeInfo
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


{-| Various ways replaying the InputHistory on the new code wouldn't make sense -}
type alias SwapError =
  String


{-| Error with () means it was already in that state -}
setPlaying : DebugSession -> Bool -> Task () ()
setPlaying =
  Native.Debugger.RuntimeApi.setPlaying


-- TODO: this should return the current values (what does that mean tho)
{-| Error with () means it was already in that state -}
setSubscribedToNode : DebugSession -> NodeId -> Bool -> Task () ()
setSubscribedToNode =
  Native.Debugger.RuntimeApi.setSubscribedToNode


-- Util

prettyPrint : JsElmValue -> String
prettyPrint val =
  Native.Debugger.RuntimeApi.prettyPrint val "  "

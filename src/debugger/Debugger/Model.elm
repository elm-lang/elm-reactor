module Debugger.Model where

import Dict exposing (Dict)
import Set exposing (Set)
import Json.Decode as JsDec exposing ((:=))
import Json.Encode as JsEnc
import Time exposing (Time)
import Utils.Helpers exposing (unsafe)

import Utils.JsArray as JsArray


type alias FrameIndex =
  Int


type alias JsElmValue =
  JsEnc.Value


type Window =
  Window


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
  , updatedNodes : List NodeId
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
  JsArray.JsArray Event


-- TODO: better name
type alias ImmediateSessionRecord =
  { sgShape : SGShape
  , modul : ElmModule
  , snapshots : JsArray.JsArray Snapshot
  , inputHistory : InputHistory
  , delay : Time
  , startedAt : Time
  , pausedAt : Time
  }


type alias ReplayError =
  { oldShape : SGShape
  , newShape : SGShape
  }


type alias SalientSGNodes =
  { main : NodeId
  , foldps : List { parent : NodeId, foldp : NodeId }
  }


{-| Get main, foldps, and foldp parents, so they can be
subscribed to & displayed. -}
getSalientNodes : SGShape -> SalientSGNodes
getSalientNodes shape =
  let
    isFoldp nodeInfo =
      case nodeInfo.nodeType of
        InternalNode ->
          nodeInfo.name == "foldp"

        _ ->
          False

    foldps =
      shape.nodes
        |> Dict.filter (\_ nodeInfo -> isFoldp nodeInfo)
        |> Dict.keys

    parentOfFoldp foldpId =
      shape.nodes
        |> Dict.filter (\id nodeInfo -> foldpId `List.member` nodeInfo.kids)
        |> Dict.toList
        |> List.head
        |> unsafe "parent not found"
        |> fst

    foldpsAndParents =
      foldps
      |> List.map (\foldpId ->
        { parent = parentOfFoldp foldpId
        , foldp = foldpId
        })
  in
    { main = shape.mainId
    , foldps = foldpsAndParents
    }


getSalientNodesAsList : SGShape -> List NodeId
getSalientNodesAsList shape =
  let
    nodes =
      getSalientNodes shape
  in
    [nodes.main] ++ (List.map .parent nodes.foldps) ++ (List.map .foldp nodes.foldps)


-- JSON CONVERSIONS


decodeSessionRecord : JsDec.Decoder SessionRecord
decodeSessionRecord =
  JsDec.object2
    SessionRecord
    ("moduleName" := JsDec.string)
    ("inputHistory" := JsArray.decode decodeEvent)


encodeSessionRecord : SessionRecord -> JsEnc.Value
encodeSessionRecord record =
  JsEnc.object
    [ ( "moduleName", JsEnc.string record.moduleName )
    , ( "inputHistory"
      , record.inputHistory |> JsArray.map encodeEvent |> JsArray.encode
      )
    ]


decodeEvent : JsDec.Decoder Event
decodeEvent =
  JsDec.object3
    Event
    ("value" := JsDec.value)
    ("nodeId" := JsDec.int)
    ("time" := JsDec.float)


encodeEvent : Event -> JsEnc.Value
encodeEvent event =
  JsEnc.object
    [ ("value", event.value)
    , ("nodeId", JsEnc.int event.nodeId)
    , ("time", JsEnc.float event.time)
    ]


logItemForFrameIdx : FrameIndex -> List (FrameIndex, a) -> Maybe a
logItemForFrameIdx idx log =
  log
    |> List.filter (\(itemIdx, val) -> itemIdx <= idx)
    |> Utils.Helpers.last
    |> Maybe.map snd

module Debugger.RuntimeApi where

import Task exposing (Task)
import Dict
import Json.Encode as JsEnc
import Time
import Empty exposing (Empty)

import DataUtils exposing (..)

import Native.Debugger.RuntimeApi


type alias FrameIndex =
  Int


type alias JsElmValue =
  JsEnc.Value


type DebugSession
  = DebugSession -- opaque


type alias NodeId =
  Int


type StateError
  = IsDisposed
  | IsPlaying


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


justMain : SGShape -> List NodeId
justMain shape =
  [shape.mainId]


getModule : DebugSession -> ElmModule
getModule =
  Native.Debugger.RuntimeApi.getModule


getAddress : DebugSession -> Signal.Address NewFrameNotification
getAddress =
  Native.Debugger.RuntimeApi.getAddress


getSubscriptions : DebugSession -> Task x (List NodeId)
getSubscriptions =
  Native.Debugger.RuntimeApi.getSubscriptions


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
getNodeState : DebugSession -> FrameInterval -> List NodeId -> Task x (List (NodeId, ValueLog))
getNodeState =
  Native.Debugger.RuntimeApi.getNodeState


getNodeStateSingle : DebugSession -> FrameIndex -> List NodeId -> Task x ValueSet
getNodeStateSingle session frameIdx nodes =
  getNodeState session {start=frameIdx, end=frameIdx} nodes
    |> Task.map (\logs ->
        logs |> List.map (\(id, log) ->
            (id, List.head log |> getMaybe "head of empty" |> snd)))


getInputHistory : DebugSession -> Task x InputHistory
getInputHistory =
  Native.Debugger.RuntimeApi.getInputHistory


emptyInputHistory : InputHistory
emptyInputHistory =
  Native.Debugger.RuntimeApi.emptyInputHistory


splitInputHistory : FrameIndex -> InputHistory -> (InputHistory, InputHistory)
splitInputHistory =
  Native.Debugger.RuntimeApi.splitInputHistory


serializeInputHistory : InputHistory -> String
serializeInputHistory =
  Native.Debugger.RuntimeApi.serializeInputHistory


parseInputHistory : String -> Result String InputHistory
parseInputHistory =
  Native.Debugger.RuntimeApi.parseInputHistory


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
initializeFullscreen : ElmModule
                    -> InputHistory
                    -> Signal.Address NewFrameNotification
                    -> (SGShape -> List NodeId)
                    -> Task x (DebugSession, ValueSet)
initializeFullscreen =
  Native.Debugger.RuntimeApi.initializeFullscreen


dispose : DebugSession -> Task x ()
dispose =
  Native.Debugger.RuntimeApi.dispose


validate : InputHistory -> SGShape -> SGShape -> Maybe SwapError
validate inputHistory oldShape newShape =
  -- TODO: filter out things from mailboxes
  Nothing


swap : DebugSession
    -> ElmModule
    -> (SGShape -> List NodeId)
    -> Task SwapError DebugSession
swap session newMod initialNodesFun =
  (dispose session)
  `Task.andThen` (\_ ->
    (getInputHistory session)
    `Task.andThen` (\history ->
      (initializeFullscreen
        newMod
        history
        (getAddress session)
        initialNodesFun
      )
      `Task.andThen` (\(newSession, _) ->
        let
          nodes =
            initialNodesFun (sgShape newSession)

          valid =
            validate history (sgShape session) (sgShape newSession)
        in
          case valid of
            Just swapErr ->
              Task.fail swapErr

            Nothing ->
              Task.succeed newSession
      )
    )
  )


forkFrom : DebugSession
        -> FrameIndex
        -> Task x (DebugSession, ValueSet)
forkFrom session frameIdx =
  (dispose session)
  `Task.andThen` (\_ ->
    (getSubscriptions session)
    `Task.andThen` (\subs ->
      (getInputHistory session
       |> Task.map (\history ->
            history |> splitInputHistory frameIdx |> fst)
      )
      `Task.andThen` (\historyUpTo ->
        (initializeFullscreen
          (getModule session)
          historyUpTo
          (getAddress session)
          (always subs)
        )
        `Task.andThen` (\(newSession, _) ->
          (getNodeStateSingle
            newSession
            frameIdx
            subs
          )
          |> Task.map (\values -> (newSession, values))
        )
      )
    )
  )


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

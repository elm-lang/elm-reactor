module Debugger.RuntimeApi where

import Task exposing (Task)
import Dict
import Json.Encode as JsEnc
import Time
import Debug

import DataUtils exposing (..)

import Native.Debugger.RuntimeApi


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


-- TODO: this is not pure! should be a task!
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
getNodeState : DebugSession -> FrameInterval -> List NodeId -> Task StateError (List (NodeId, ValueLog))
getNodeState =
  Native.Debugger.RuntimeApi.getNodeState


getNodeStateSingle : DebugSession -> FrameIndex -> List NodeId -> Task StateError ValueSet
getNodeStateSingle session frameIdx nodes =
  getNodeState session {start=frameIdx, end=frameIdx} nodes
    |> Task.map (\logs ->
        logs |> List.map (\(id, log) ->
            (id, List.head log |> getMaybe "head of empty" |> snd)))


-- INPUT HISTORY (maybe all these should live in a different module?)


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


parseInputHistory : String -> Result InputHistoryParseError InputHistory
parseInputHistory =
  Native.Debugger.RuntimeApi.parseInputHistory


type InputHistoryParseError
  = JsonParseError String
  | JsonSchemaError


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
                    -> Signal.Address NewFrameNotification
                    -> (SGShape -> List NodeId)
                    -> Task x (DebugSession, ValueSet)
initializeFullscreen =
  Native.Debugger.RuntimeApi.initializeFullscreen


dispose : DebugSession -> Task x ()
dispose =
  Native.Debugger.RuntimeApi.dispose


{-| Set the module's event history to be the given history and
regenerate snapshots. Failure means the event history wasn't empty
(this is intended to be used right after initialization)

Currently returns flagged expr history but not node history. -}
setInputHistory : DebugSession -> InputHistory -> Task () (List (ExprTag, ValueLog))
setInputHistory =
  Native.Debugger.RuntimeApi.setInputHistory


validate : InputHistory -> SGShape -> SGShape -> Maybe SwapError
validate inputHistory oldShape newShape =
  -- TODO: filter out things from mailboxes
  Nothing


{-| Given current session and new module:

- kill old module
- initialize new module
- check if the old module's event history can be replayed over the new module
  (failing if no)
- replay events over old module, saving snapshots along the way
- return new session and logs for all flagged expressions (`Debug.log`)
-}
swap : DebugSession
    -> ElmModule
    -> (SGShape -> List NodeId)
    -> Task SwapError (DebugSession, List (ExprTag, ValueLog))
swap session newMod initialNodesFun =
  (dispose session)
  `Task.andThen` (\_ ->
    (getInputHistory session)
    `Task.andThen` (\history ->
      (initializeFullscreen
        newMod
        (getAddress session)
        initialNodesFun)
      `Task.andThen` (\(newSession, _) ->
        let valid =
          validate history (sgShape session) (sgShape newSession)
        in
          case valid of
            Just swapErr ->
              Task.fail swapErr

            Nothing ->
              setInputHistory newSession history
                |> Task.mapError (\_ -> Debug.crash "session's event list wasn't empty")
                |> Task.map (\logs -> (newSession, logs))
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
            history |> splitInputHistory frameIdx |> fst))
      `Task.andThen` (\historyUpTo ->
        (initializeFullscreen
          (getModule session)
          (getAddress session)
          (always subs))
        `Task.andThen` (\(newSession, _) ->
          (setInputHistory
            newSession
            historyUpTo
          |> Task.mapError (\_ -> Debug.crash "session's event list wasn't empty"))
          `Task.andThen` (\_ ->
            (getNodeStateSingle
              newSession
              frameIdx
              subs)
            |> Task.mapError (Debug.crash << toString)
            |> Task.map (\values -> (newSession, values))
          )
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

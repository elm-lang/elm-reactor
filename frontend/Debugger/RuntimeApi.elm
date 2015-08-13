module Debugger.RuntimeApi where

import Task exposing (Task)
import Dict
import Json.Encode as JsEnc
import Time exposing (Time)
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
  , time : Time
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
getNumFrames : DebugSession -> Task x Int
getNumFrames =
  Native.Debugger.RuntimeApi.getNumFrames


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


getInputHistory : DebugSession -> InputHistory
getInputHistory =
  Native.Debugger.RuntimeApi.getInputHistory


emptyInputHistory : InputHistory
emptyInputHistory =
  Native.Debugger.RuntimeApi.emptyInputHistory


splitInputHistory : FrameIndex -> InputHistory -> Task x (InputHistory, InputHistory)
splitInputHistory =
  Native.Debugger.RuntimeApi.splitInputHistory


serializeInputHistory : InputHistory -> Task x String
serializeInputHistory =
  Native.Debugger.RuntimeApi.serializeInputHistory


parseInputHistory : String -> Result InputHistoryParseError InputHistory
parseInputHistory =
  Native.Debugger.RuntimeApi.parseInputHistory


type alias EventIdx =
  Int


getEvent : InputHistory -> EventIdx -> Task x (Maybe Event)
getEvent =
  Native.Debugger.RuntimeApi.getEvent


getHistoryModuleName : InputHistory -> ModuleName
getHistoryModuleName =
  Native.Debugger.RuntimeApi.getHistoryModuleName


type InputHistoryParseError
  = JsonParseError String
  | JsonSchemaError String


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

{-| Starts off playing.
Subscribes to the list of nodes returned by the given function (3rd arg),
and returns their initial values. -}
initializeFullscreen : ElmModule
                    -> Time
                    -> Signal.Address NewFrameNotification
                    -> Task x DebugSession
initializeFullscreen =
  Native.Debugger.RuntimeApi.initializeFullscreen


initializeFullscreenAndSubscribe : ElmModule
                                -> Time
                                -> Signal.Address NewFrameNotification
                                -> (SGShape -> List NodeId)
                                -> Task x (DebugSession, ValueSet)
initializeFullscreenAndSubscribe modul delay addr initialNodesFun =
  (initializeFullscreen modul delay addr)
  `Task.andThen` (\newSession ->
    (newSession
      |> sgShape
      |> initialNodesFun
      |> List.map (\nodeId -> setSubscribedToNode newSession nodeId True)
      |> Task.sequence
      |> Task.mapError (\_ -> Debug.crash "already subscribed"))
    `Task.andThen` (\_ ->
      (getNodeStateSingle newSession 0 (newSession |> sgShape |> initialNodesFun)
        |> Task.mapError (Debug.crash << toString))
      `Task.andThen` (\valueSet ->
        Task.succeed (newSession, valueSet)
      )
    )
  )


{-| Removes all event handlers and removes the session's node from the DOM. -}
dispose : DebugSession -> Task x ()
dispose =
  Native.Debugger.RuntimeApi.dispose


{-| Forces the module to render the given value (expected to be Html
or Element). -}
setMain : DebugSession -> JsElmValue -> Task x ()
setMain =
  Native.Debugger.RuntimeApi.setMain


{-| Set the module's event history to be the given history and
regenerate snapshots. Failure means the event history wasn't empty
(this is intended to be used right after initialization)

Currently returns flagged expr history but not node history. -}
replayInputHistory : DebugSession -> InputHistory -> Task () (List (ExprTag, ValueLog))
replayInputHistory =
  Native.Debugger.RuntimeApi.replayInputHistory


{-| given shape of old graph, history, and shape of new graph,
possibly return errors -}
validate : InputHistory -> SGShape -> SGShape -> Maybe ReplayError
validate inputHistory oldShape newShape =
  -- TODO: filter out things from mailboxes
  let
    d = Debug.log "(IH,EQ,OS,NS)" (inputHistory, oldShape == newShape, oldShape, newShape)
  in
    if oldShape == newShape then
      Nothing
    else
      Just { oldShape = oldShape, newShape = newShape }


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
    -> Task ReplayError (DebugSession, List (ExprTag, ValueLog))
swap session newMod initialNodesFun =
  (dispose session)
  `Task.andThen` (\_ ->
    (getInputHistory session)
    `Task.andThen` (\history ->
      (initializeFullscreenAndSubscribe
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
              replayInputHistory newSession history
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
        (initializeFullscreenAndSubscribe
          (getModule session)
          (getAddress session)
          (always subs))
        `Task.andThen` (\(newSession, _) ->
          (replayInputHistory
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


{-| Fails with an error message if the given name is not in scope -}
getFromGlobalScope : ModuleName -> Task String ElmModule
getFromGlobalScope =
  Native.Debugger.RuntimeApi.getFromGlobalScope


evalCompiledModule : CompiledElmModule -> Task x ()
evalCompiledModule =
  Native.Debugger.RuntimeApi.evalCompiledModule


instantiateModule : CompiledElmModule -> Task x ElmModule
instantiateModule compiled =
  (evalCompiledModule compiled)
  `Task.andThen` (\_ ->
    getFromGlobalScope compiled.name
      |> Task.mapError (\err -> Debug.crash <| "name wasn't in scope: " ++ err)
  )


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


{-| Means replaying the InputHistory on the new code wouldn't make sense -}
type alias ReplayError
  = { oldShape : SGShape
    , newShape : SGShape
    }


{-| Pauses the module (all incoming events will be ignored), returning
the time that it was paused.

Error with () means it was already in that state -}
pause : DebugSession -> Task () Time
pause =
  Native.Debugger.RuntimeApi.pause


-- TODO: this should return the current values (what does that mean tho)
{-| Error with () means it was already in that state -}
setSubscribedToNode : DebugSession -> NodeId -> Bool -> Task () ()
setSubscribedToNode =
  Native.Debugger.RuntimeApi.setSubscribedToNode


-- Util

prettyPrint : JsElmValue -> String
prettyPrint val =
  Native.Debugger.RuntimeApi.prettyPrint val "  "

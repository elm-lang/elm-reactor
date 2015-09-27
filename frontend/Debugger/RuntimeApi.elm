module Debugger.RuntimeApi where

import Task exposing (Task)
import Dict
import Set
import Debug

import DataUtils exposing (..)
import Debugger.Model exposing (..)
import JsArray

import Native.Debugger.RuntimeApi


---- COMMANDS ----

-- starting, pausing, disposing

start : Window
     -> ElmModule
     -> Signal.Address NewFrameNotification
     -> (SGShape -> List NodeId)
     -> Task x (DebugSession, ValueSet)
start =
  Native.Debugger.RuntimeApi.start


swap : Window
    -> ElmModule
    -> Signal.Address NewFrameNotification
    -> (SGShape -> List NodeId)
    -> InputHistory
    -> Maybe SGShape
    -> (SGShape -> SGShape -> InputHistory -> Bool)
    -> Task ReplayError (DebugSession, List (ExprTag, ValueLog), List (NodeId, ValueLog))
swap =
  Native.Debugger.RuntimeApi.swap


shapesEqual : SGShape -> SGShape -> InputHistory -> Bool
shapesEqual old new history =
  old == new


{-| Initialize module with events and snapshots from the
SessionRecord. The new instance's delay is calculated from
the SessionRecord's paused time and the current time.

Error: SessionRecord's name is different than given module's. -}
play : Window
    -> ImmediateSessionRecord
    -> Signal.Address NewFrameNotification
    -> (SGShape -> List NodeId)
    -> Task x (DebugSession, ValueSet)
play =
  Native.Debugger.RuntimeApi.play


{-| Module will now ignore all inputs, but can still be used
for getNodeState -}
pause : DebugSession -> Task () ImmediateSessionRecord
pause =
  Native.Debugger.RuntimeApi.pause


{-| Removes the session's node from the DOM; it can no longer
be used for getNodeState. -}
dispose : DebugSession -> Task x ()
dispose =
  Native.Debugger.RuntimeApi.dispose


-- getting and rendering past node state


-- TODO: this should return the current values (what does that mean tho)
{-| Error with () means it was already in that state -}
setSubscribedToNode : DebugSession -> NodeId -> Bool -> Task () ()
setSubscribedToNode =
  Native.Debugger.RuntimeApi.setSubscribedToNode


subscribeToAll : DebugSession -> (SGShape -> List NodeId) -> Task () ()
subscribeToAll session nodesFun =
  session
    |> getSgShape
    |> nodesFun
    |> List.map (\nodeId -> setSubscribedToNode session nodeId True)
    |> Task.sequence
    |> Task.map (always ())


mainAndFoldpParents : SGShape -> List NodeId
mainAndFoldpParents shape =
  let
    isFoldp nodeInfo =
      case nodeInfo.nodeType of
        InternalNode ->
          nodeInfo.name == "foldp"

        _ ->
          False

    foldpIds =
      shape.nodes
        |> Dict.filter (\_ nodeInfo -> isFoldp nodeInfo)
        |> Dict.keys
        |> Set.fromList

    parentOfAFoldp nodeInfo =
      nodeInfo.kids
        |> Set.fromList
        |> Set.intersect foldpIds
        |> Set.isEmpty
        |> not

    foldpParents =
      shape.nodes
        |> Dict.filter (\_ nodeInfo -> parentOfAFoldp nodeInfo)
        |> Dict.keys
  in
    shape.mainId :: foldpParents


{-| Forces the module to render the given value (expected to be Html
or Element). -}
renderMain : DebugSession -> JsElmValue -> Task x ()
renderMain =
  Native.Debugger.RuntimeApi.renderMain


-- QUERIES

getSgShape : DebugSession -> SGShape
getSgShape =
  Native.Debugger.RuntimeApi.getSgShape


getModule : DebugSession -> ElmModule
getModule =
  Native.Debugger.RuntimeApi.getModule


getAddress : DebugSession -> Signal.Address NewFrameNotification
getAddress =
  Native.Debugger.RuntimeApi.getAddress


getSubscriptions : DebugSession -> Task x (List NodeId)
getSubscriptions =
  Native.Debugger.RuntimeApi.getSubscriptions


getSessionRecord : DebugSession -> Task x SessionRecord
getSessionRecord =
  Native.Debugger.RuntimeApi.getSessionRecord


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


-- GETTING MODULE FROM JS


{-| Fails with an error message if the given name is not in scope -}
getFromGlobalScope : Window -> ModuleName -> Task String ElmModule
getFromGlobalScope =
  Native.Debugger.RuntimeApi.getFromGlobalScope


evalCompiledModule : Window -> CompiledElmModule -> Task x ()
evalCompiledModule =
  Native.Debugger.RuntimeApi.evalCompiledModule


instantiateModule : Window -> CompiledElmModule -> Task x ElmModule
instantiateModule window_ compiled =
  (evalCompiledModule window_ compiled)
  `Task.andThen` (\_ ->
    getFromGlobalScope window_ compiled.name
      |> Task.mapError (\err -> Debug.crash <| "name wasn't in scope: " ++ err)
  )


opener : Window
opener =
  Native.Debugger.RuntimeApi.opener


splitRecord : FrameIndex
           -> ImmediateSessionRecord
           -> (ImmediateSessionRecord, ImmediateSessionRecord)
splitRecord idx record =
  let
    (beforeHistory, afterHistory) =
      JsArray.split idx record.inputHistory

    snapshotIdx =
      idx // Native.Debugger.RuntimeApi.eventsPerSnapshot + 1

    (beforeSnaps, afterSnaps) =
      JsArray.split snapshotIdx record.snapshots
  in
    ( { record
          | inputHistory <- beforeHistory
          , snapshots <- beforeSnaps
          , pausedAt <-
              JsArray.get -1 beforeHistory
                |> Maybe.map .time
                |> Maybe.map (\x -> x + record.delay)
                |> Maybe.withDefault record.startedAt
      }
    , { record
          | inputHistory <- afterHistory
          , snapshots <- afterSnaps
      }
    )
      


-- PRETTY PRINT
-- TODO: return Elm repr of Elm value

prettyPrint : JsElmValue -> String
prettyPrint val =
  Native.Debugger.RuntimeApi.prettyPrint val

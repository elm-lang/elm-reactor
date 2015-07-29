module Model where

import Json.Decode exposing (..)
import WebSocket

import Debugger.Model as DM
import SideBar.Logs as Logs
import Button
import Debugger.RuntimeApi as API

type alias Model =
  { serviceState : DM.Model
  , sidebarVisible : Bool
  , permitSwaps : Bool
  , restartButtonState : Button.Model
  , playPauseButtonState : Button.Model
  , logsState : Logs.Model
  , swapSocket : Maybe WebSocket.WebSocket
  }


initModel : Model
initModel =
  { serviceState = DM.Uninitialized
  , sidebarVisible = True
  , permitSwaps = True
  , restartButtonState = Button.Up
  , playPauseButtonState = Button.Up
  , logsState = Logs.initModel
  , swapSocket = Nothing
  }


type Action
  = SidebarVisible Bool
  | PermitSwaps Bool
  | NewServiceState DM.Model
  -- TODO: vv get rid of these with new component arch ...? vv
  | PlayPauseButtonAction Button.Action
  | RestartButtonAction Button.Action
  | LogsAction Logs.Action
  | ConnectSocket (Maybe WebSocket.WebSocket)
  | SwapEvent SwapEvent
  | NoOp


type alias CompilationErrors =
  String


type SwapEvent
  = NewModule API.CompiledElmModule
  | CompilationErrors CompilationErrors


swapEvent : Decoder SwapEvent
swapEvent =
  oneOf
    [ object2
        (\name code ->
            NewModule {name=name, code=code})
        ("name" := string)
        ("code" := string)
    , object1
        CompilationErrors
        ("error" := string)
    ]

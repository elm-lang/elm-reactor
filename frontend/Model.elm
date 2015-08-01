module Model where

import Json.Decode exposing (..)
import WebSocket

import Debugger.Service as DS
import SideBar.Logs as Logs
import Button
import Debugger.RuntimeApi as API

type alias Model =
  { serviceState : DS.Model
  , sidebarVisible : Bool
  , permitSwaps : Bool
  , restartButtonState : Button.Model
  , playPauseButtonState : Button.Model
  , logsState : Logs.Model
  , swapSocket : Maybe WebSocket.WebSocket
  }


initModel : Model
initModel =
  { serviceState = Nothing
  , sidebarVisible = True
  , permitSwaps = True
  , restartButtonState = Button.Up
  , playPauseButtonState = Button.Up
  , logsState = Logs.initModel
  , swapSocket = Nothing
  }


type Message
  = SidebarVisible Bool
  | PermitSwaps Bool
  | NewServiceState DS.Model
  -- TODO: vv get rid of these with new component arch ...? vv
  | PlayPauseButtonAction Button.Message
  | RestartButtonAction Button.Message
  | LogsAction Logs.Message
  | ConnectSocket (Maybe WebSocket.WebSocket)
  | SwapEvent SwapEvent
  | ServiceMessage Service.Message
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

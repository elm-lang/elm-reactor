module Model where

import Json.Decode exposing (..)

import WebSocket
import Html.File

import Debugger.Service as Service
import Debugger.Active as Active
import SideBar.Logs as Logs
import Button
import Debugger.RuntimeApi as API


type alias Model =
  { serviceState : Service.Model
  , sidebarVisible : Bool
  , permitSwaps : Bool
  , restartButtonState : Button.Model
  , playPauseButtonState : Button.Model
  , compilationErrors : Maybe CompilationErrors
  , logsState : Logs.Model
  , swapSocket : Maybe WebSocket.WebSocket
  }


initModel : Model
initModel =
  { serviceState = Service.initModel
  , sidebarVisible = True
  , permitSwaps = True
  , restartButtonState = Button.Up
  , playPauseButtonState = Button.Up
  , compilationErrors = Nothing
  , logsState = Logs.initModel
  , swapSocket = Nothing
  }


type Message
  = SidebarVisible Bool
  | PermitSwaps Bool
  | NewServiceState Service.Model
  | PlayPauseButtonAction (Button.Message Active.Command)
  | RestartButtonAction (Button.Message Active.Command)
  | LogsMessage Logs.Message
  | ConnectSocket (Maybe WebSocket.WebSocket)
  | SwapEvent SwapEvent
  | ServiceCommand Active.Command
  | ExportHistory
  | FilesDropped (List Html.File.File)
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

module Model where

import Debugger.Model as DM
import SideBar.Logs as Logs
import Button

type alias Model =
  { serviceState : DM.Model
  , sidebarVisible : Bool
  , permitSwaps : Bool
  , restartButtonState : Button.Model
  , playPauseButtonState : Button.Model
  , logsState : Logs.Model
  }


initModel : Model
initModel =
  { serviceState = DM.Uninitialized
  , sidebarVisible = True
  , permitSwaps = True
  , restartButtonState = Button.Up
  , playPauseButtonState = Button.Up
  , logsState = Logs.initModel
  }


type Action
  = SidebarVisible Bool
  | PermitSwaps Bool
  | NewServiceState DM.Model
  | CompilationErrors CompilationErrors
  -- TODO: vv get rid of these with new component arch ...? vv
  | PlayPauseButtonAction Button.Action
  | RestartButtonAction Button.Action
  | LogsAction Logs.Action


type alias CompilationErrors =
  String

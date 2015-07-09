module OverlayModel where

import Button

type alias Model =
    { sidebarVisible : Bool
    , restartButtonState : Button.Model
    , playPauseButtonState : Button.Model
    }


type Action
    = SidebarVisible Bool
    | RestartButtonAction Button.Action
    | PlayPauseButtonAction Button.Action


initState : Model
initState =
  { sidebarVisible = True
  , restartButtonState = Button.Up
  , playPauseButtonState = Button.Up
  }


update : Action -> Model -> Model
update action state =
  case action of
    RestartButtonAction buttonAct ->
      { state |
          restartButtonState <- Button.update buttonAct state.restartButtonState
      }

    PlayPauseButtonAction buttonAct ->
      { state |
          playPauseButtonState <- Button.update buttonAct state.restartButtonState
      }

    SidebarVisible visible ->
      { state |
          sidebarVisible <- visible
      }

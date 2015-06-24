module SideBar.Model where

import Button


type Action
    = Restart
    | Pause Bool
    | TotalEvents Int
    | ScrubPosition Int
    | RestartButtonAction Button.Action
    | PlayPauseButtonAction Button.Action
    | NoOp

type alias Model =
    { paused : Bool
    , totalEvents : Int
    , scrubPosition : Int
    , restartButtonState : Button.Model
    , playPauseButtonState : Button.Model
    }


startModel : Model
startModel =
    { paused = False
    , totalEvents = 0
    , scrubPosition = 0
    , restartButtonState = Button.Up
    , playPauseButtonState = Button.Up
    }


update : Action -> Model -> Model
update action state =
  case action of
    Restart ->
        { state |
            totalEvents <- 0,
            scrubPosition <- 0
        }

    Pause pause ->
        { state |
            paused <- pause,
            totalEvents <-
                if pause then state.totalEvents else state.scrubPosition
        }

    TotalEvents n ->
        { state |
            totalEvents <- n,
            scrubPosition <- n
        }

    ScrubPosition pos ->
        { state |
            scrubPosition <- pos,
            paused <- True
        }

    RestartButtonAction buttonAct ->
        { state |
            restartButtonState <- Button.update buttonAct state.restartButtonState
        }

    PlayPauseButtonAction buttonAct ->
        { state |
            playPauseButtonState <- Button.update buttonAct state.restartButtonState
        }


module SideBar.Model where


type Action
    = Restart
    | Pause Bool
    | TotalEvents Int
    | ScrubPosition Int
    | RestartButtonState ButtonState
    | PlayPauseButtonState ButtonState

type ButtonState = Up | Hover | Down

type alias Model =
    { paused : Bool
    , totalEvents : Int
    , scrubPosition : Int
    , restartButtonState : ButtonState
    , playPauseButtonState : ButtonState
    }


startModel : Model
startModel =
    { paused = False
    , totalEvents = 0
    , scrubPosition = 0
    , restartButtonState = Up
    , playPauseButtonState = Up
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

    RestartButtonState buttonSt ->
        { state |
            restartButtonState <- buttonSt
        }

    PlayPauseButtonState buttonSt ->
        { state |
            playPauseButtonState <- buttonSt
        }


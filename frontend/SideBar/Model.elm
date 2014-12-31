module SideBar.Model where


type Action
    = Restart
    | Pause Bool
    | TotalEvents Int
    | ScrubPosition Int


type alias Model =
    { paused : Bool
    , totalEvents : Int
    , scrubPosition : Int
    }


startModel : Model
startModel =
    { paused = False
    , totalEvents = 0
    , scrubPosition = 0
    }


update : Action -> Model -> Model
update update state =
  case update of
    Restart ->
        startModel

    Pause doPause ->
        { state |
            paused <- doPause,
            totalEvents <-
                if doPause then state.totalEvents else state.scrubPosition
        }

    TotalEvents events ->
        { state |
            totalEvents <- events,
            scrubPosition <- events
        }

    ScrubPosition pos ->
        { state |
            scrubPosition <- pos,
            paused <- True
        }


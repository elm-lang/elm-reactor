module Model where

type Update
    = Restart
    | Pause Bool
    | TotalEvents Int
    | ScrubPosition Int

type alias State =
    { paused : Bool
    , totalEvents : Int
    , scrubPosition : Int
    }

startState : State
startState =
    { paused = False
    , totalEvents = 0
    , scrubPosition = 0
    }

step : Update -> State -> State
step update state =
    case update of
        Restart ->
            startState

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


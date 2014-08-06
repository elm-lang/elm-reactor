module DebuggerInterface where

import Window
import Graphics.Input (..)
import Debug
import Slider (..)

-- Model

data Update
    = Restart
    | Pause Bool
    | TotalEvents Int
    | ScrubPosition Int

type State =
    { paused : Bool
    , totalEvents : Int
    , scrubPosition : Int
    }

-- View

objHeight = 40
buttonWidth = 40
panelWidth = 275

blue = rgb 60 160 255
lightGrey = rgb 233 233 233
darkGrey = rgb 92 92 92

playButton : Element
playButton =
    let icon = [ngon 3 15.0 |> filled blue]
    in  collage buttonWidth objHeight icon
            |> clickable pausedInput.handle False

pauseButton : Element
pauseButton =
    let icon = [ rect 7.5 20
                    |> filled blue
                    |> moveX -6
               , rect 7.5 20
                    |> filled blue
                    |> moveX 6
                ]
    in collage buttonWidth objHeight icon
            |> clickable pausedInput.handle True

restartButton : Element
restartButton =
    let icon = [circle 15.0 |> filled lightGrey]
    in  collage buttonWidth objHeight icon
            |> clickable restartInput.handle ()

scrubSlider : (Int, Int) -> State -> Element
scrubSlider (w,_) state =
    let sliderLength = w
        sliderStyle =
            { defaultSlider
            | length <- sliderLength
            , max <- toFloat <| state.totalEvents
            , value <- toFloat <| state.scrubPosition
            , disabled <- not state.paused
            }
    in  container sliderLength objHeight midLeft
            <| slider scrubInput.handle round sliderStyle

view : (Int, Int) -> State -> Element
view (w,h) state =
    let sideMargin = (2 * 20)
        spacerHeight = 15
        controlsHeight = objHeight + 24 + spacerHeight + 10
        controls =
            container w controlsHeight midTop <|
                spacer (w - sideMargin) spacerHeight
                `above` (container (w - sideMargin) controlsHeight midTop 
                <| restartButton
                `beside` spacer (panelWidth - 2 * buttonWidth - sideMargin) objHeight
                `beside` (if state.paused then playButton else pauseButton)
                `above` scrubSlider (w - sideMargin,h) state)
    in  controls
        `above` [markdown| <br /> |]
        `above` asText "watches not implemented yet :("


-- The wiring

main : Signal Element
main = lift2 view Window.dimensions scene

port scrubTo : Signal Int
port scrubTo = scrubInput.signal

port pause : Signal Bool
port pause = pausedInput.signal

port restart : Signal Int
port restart = lift (\x -> 0) restartInput.signal

pausedInput : Input Bool
pausedInput = input False

restartInput : Input ()
restartInput = input ()

scrubInput : Input Int
scrubInput = input 0

port eventCounter : Signal Int

scene : Signal State
scene = foldp step startState aggregateUpdates

startState : State
startState =
    { paused = False
    , totalEvents = 0
    , scrubPosition = 0
    }

step : Update -> State -> State
step update state = case update of
    Restart ->
        startState
    Pause doPause ->
        { state | paused <- doPause }
    TotalEvents events ->
        { state | totalEvents <- events
                , scrubPosition <- events}
    ScrubPosition pos ->
        { state | scrubPosition <- pos}

aggregateUpdates : Signal Update
aggregateUpdates = merges
    [ always Restart <~ restartInput.signal
    , Pause <~ pausedInput.signal
    , TotalEvents <~ eventCounter
    , ScrubPosition <~ scrubInput.signal
    ]

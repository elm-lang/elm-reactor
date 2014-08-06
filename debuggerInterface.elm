module DebuggerInterface where

import Window
import Graphics.Input (..)
import Text (..)
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

blue = rgb 49 139 255
lightGrey = rgb 228 228 228
darkGrey = rgb 74 74 74

playButton : Element
playButton =
    let icon =
            [ rect 35 35 |> filled blue
            , ngon 3 12.0 |> filled lightGrey
            ]
    in  collage buttonWidth objHeight icon
            |> clickable pausedInput.handle False

pauseButton : Element
pauseButton =
    let icon =
            [ rect 35 35 |> filled blue
            , rect 7 17
                |> filled lightGrey
                |> moveX -5
           , rect 7 17
                |> filled lightGrey
                |> moveX 5
            ]
    in collage buttonWidth objHeight icon
            |> clickable pausedInput.handle True

restartButton : Element
restartButton =
    let icon =
            [ rect 35 35 |> filled lightGrey
            , circle 12.0 |> filled darkGrey
            ]
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
    in  container sliderLength objHeight bottomLeft
            <| slider scrubInput.handle round sliderStyle

view : (Int, Int) -> State -> Element
view (w,h) state =
    let sideMargin = (2 * 20)
        spacerHeight = 15
        textHeight = 30
        controlsHeight = objHeight + 24 + spacerHeight + textHeight + 10
        buttons = flow right
            [ restartButton
            , spacer (panelWidth - 2 * buttonWidth - sideMargin) objHeight
            , (if state.paused then playButton else pauseButton)
            ]
        slider = flow down
            [ scrubSlider (w - sideMargin, h) state
            , sliderBottomText
            ]
        sliderBottomText = flow outward
            [ sliderStartText
            , sliderTotalEvents
            ]
        sliderStartText = container (w-sideMargin) textHeight midLeft
            (textStyle "0" |> leftAligned)
        sliderTotalEvents = container (w-sideMargin) textHeight midRight
            (show state.totalEvents |> textStyle |> rightAligned)
        textStyle =
            style
                { defaultStyle
                | typeface <- ["Gotham", "sans-serif"]
                , color <- lightGrey
                , height <- Just 11
                }
            . toText
        controlsContainer = container w controlsHeight midTop centeredControls
        centeredControls = flow down
            [ spacer (w - sideMargin) spacerHeight
            , container (w - sideMargin) controlsHeight midTop controls
            ]
        controls = flow down
            [ buttons
            , slider
            ]
    in  flow down
            [ controlsContainer
            --, [markdown| <hr/> |]
            --, asText "watches not implemented yet :("
            ]
        


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

module DebuggerInterface where

import Window
import Graphics.Input (..)
import Graphics.Element as GE
import Text (..)
import Debug
import Slider (..)
import Json (..)

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

textStyle : String -> Text
textStyle =
    style
        { defaultStyle
        | typeface <- ["Gotham", "sans-serif"]
        , color <- lightGrey
        , height <- Just 11
        }
    . toText

roundedSquare : Float -> Float -> (Shape -> Form) -> Form
roundedSquare side radius toForm =
    let shortSide = side - 2 * radius
        xRect = rect side shortSide |> toForm
        yRect = rect shortSide side |> toForm
        circleOffset = shortSide / 2
        formedCircle = circle radius |> toForm
        tl = formedCircle |> move (-circleOffset, circleOffset)
        tr = formedCircle |> move ( circleOffset, circleOffset)
        bl = formedCircle |> move (-circleOffset,-circleOffset)
        br = formedCircle |> move ( circleOffset,-circleOffset)
    in group [xRect, yRect, tl, tr, bl, br]

playButton : Element
playButton =
    let icon =
            [ roundedSquare 35 3 (filled blue)
            , ngon 3 12.0 |> filled lightGrey
            ]
    in  collage buttonWidth objHeight icon
            |> clickable pausedInput.handle False

pauseButton : Element
pauseButton =
    let icon =
            [ roundedSquare 35 3 (filled blue)
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
            [ roundedSquare 35 3 (filled lightGrey)
            , circle 12.0 |> filled darkGrey
            , circle 8 |> filled lightGrey
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
    in  container sliderLength 20 midLeft
            <| slider scrubInput.handle round sliderStyle

sliderCurrentEvent : Int -> State -> Element
sliderCurrentEvent w state =
    let textHeight = 20
        displayPercent = 0.85
        scrubPosition = toFloat state.scrubPosition
        totalEvents = toFloat state.totalEvents
        w' = toFloat w
        leftDistance = scrubPosition / totalEvents * w' * displayPercent + (w' * (1 - displayPercent)/2)
        xPos = absolute (round leftDistance)
        yPos = absolute (round (textHeight / 2))
        textPosition = middleAt xPos yPos
        text' = show state.scrubPosition |> textStyle |> rightAligned
    in  container w textHeight textPosition text'

--showWatches : Value -> Element
--showWatches json =

view : (Int, Int) -> Value -> State -> Element
view (w,h) watches state =
    let sideMargin = (2 * 20)
        spacerHeight = 15
        textHeight = 30
        controlsHeight = objHeight + 24 + spacerHeight + textHeight + 20
        buttons = flow right
            [ restartButton
            , spacer (panelWidth - 2 * buttonWidth - sideMargin) objHeight
            , (if state.paused then playButton else pauseButton)
            ]
        slider = flow down
            [ sliderCurrentEvent (w - sideMargin) state
            , scrubSlider (w - sideMargin, h) state
            , sliderBottomText
            ]
        sliderBottomText = flow outward
            [ sliderStartText
            , sliderTotalEvents
            ]
        sliderStartText = container (w-sideMargin) textHeight topLeft
            (textStyle "0" |> leftAligned)
        sliderTotalEvents = container (w-sideMargin) textHeight topRight
            (show state.totalEvents |> textStyle |> rightAligned)
        controlsContainer = container w controlsHeight midTop centeredControls
        centeredControls = flow down
            [ spacer (w - sideMargin) spacerHeight
            , container (w - sideMargin) controlsHeight midTop controls
            ]
        controls = flow down
            [ buttons
            , slider
            ]
        bar = spacer w 1 |> GE.color lightGrey |> opacity 0.3
    in  flow down
            [ controlsContainer
            , bar
            , (asText watches) |> width w
            ]
        


-- The wiring

main : Signal Element
main = lift3 view Window.dimensions watches scene

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

port watches : Signal Value

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

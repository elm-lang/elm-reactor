module DebuggerInterface where

import Window
import Graphics.Input (..)
import Graphics.Element as GE
import Text (..)
import Debug
import Slider (..)
import Dict as D
import Json

--
-- Model
--

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

--
-- View
--

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

hotswapButton : Bool -> Element
hotswapButton permitHotswap =
    let bgButton = roundedSquare 24 2 (filled lightGrey)
        trueButton = [bgButton, roundedSquare 22 2 (filled blue)]
        falseButton = [bgButton, roundedSquare 22 2 (filled darkGrey)]
        buttonElem =
            if  | permitHotswap -> trueButton
                | otherwise -> falseButton
        hsButton = collage 25 25 buttonElem
            |> clickable permitHotswapInput.handle (not permitHotswap)
        info = "hotswap" |> textStyle |> leftAligned
    in  flow right [ info, spacer 10 1, hsButton ]

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
    in  slider scrubInput.handle round sliderStyle
            |> container sliderLength 20 midLeft

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

view : (Int, Int) -> Json.Value -> Bool -> State -> Element
view (w,h) watches permitHotswap state =
    let sideMargin = (2 * 20)
        midWidth = w - sideMargin
        spacerHeight = 15
        textHeight = 30
        controlsHeight = objHeight + 24 + spacerHeight + textHeight + 20
        fittedHotSwapButton =
            hotswapButton permitHotswap
            |> container (panelWidth - 2 * buttonWidth - sideMargin) objHeight middle
        buttons = flow right
            [ restartButton
            , fittedHotSwapButton
            , (if state.paused then playButton else pauseButton)
            ]
        slider = flow down
            [ sliderCurrentEvent midWidth state
            , scrubSlider (midWidth, h) state
            , sliderBottomText
            ]
        sliderBottomText = flow outward
            [ sliderStartText
            , sliderTotalEvents
            ]
        sliderStartText = container midWidth textHeight topLeft
            (textStyle "0" |> leftAligned)
        sliderTotalEvents = container midWidth textHeight topRight
            (show state.totalEvents |> textStyle |> rightAligned)
        controlsContainer = container w controlsHeight midTop centeredControls
        centeredControls = flow down
            [ spacer midWidth spacerHeight
            , container midWidth controlsHeight midTop controls
            ]
        controls = flow down
            [ buttons
            , slider
            ]
        bar = spacer w 1 |> GE.color lightGrey |> opacity 0.3
    in  flow down
            [ controlsContainer
            , bar
            , showValue watches |> textStyle |> leftAligned |> width w
            ]
        

--
-- The wiring
--

main : Signal Element
main = view <~ Window.dimensions
             ~ watches
             ~ permitHotswapInput.signal
             ~ scene

port scrubTo : Signal Int
port scrubTo = scrubInput.signal

port pause : Signal Bool
port pause = pausedInput.signal

port restart : Signal Int
port restart = lift (\x -> 0) restartInput.signal

port permitHotswap : Signal Bool
port permitHotswap = permitHotswapInput.signal

pausedInput : Input Bool
pausedInput = input False

permitHotswapInput : Input Bool
permitHotswapInput = input True

restartInput : Input ()
restartInput = input ()

scrubInput : Input Int
scrubInput = input 0

port eventCounter : Signal Int

port watches : Signal Json.Value

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

--
-- Utilities
--

showValue : Json.Value -> String
showValue value = Json.toString "  " value

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

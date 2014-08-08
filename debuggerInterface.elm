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
-- Style
--

objHeight = 40
buttonWidth = 40
panelWidth = 275
sideMargin = 2 * 20
textHeight = 20

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

--
-- View
--

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

sliderEventText : Int -> State -> Element
sliderEventText w state =
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

sliderInfoText : Int -> State -> Element
sliderInfoText w state =
    let sliderStartText = container w textHeight topLeft
            (textStyle "0" |> leftAligned)
        sliderTotalEvents = container w textHeight topRight
            (show state.totalEvents |> textStyle |> rightAligned)
    in  flow outward
            [ sliderStartText
            , sliderTotalEvents
            ]

view : (Int, Int) -> String -> Bool -> State -> Element
view (w,h) watches permitHotswap state =
    let midWidth = w - sideMargin
        spacerHeight = 15
        controlsHeight = objHeight + 24 + spacerHeight + 2 * textHeight
        fittedHotSwapButton =
            hotswapButton permitHotswap
            |> container (panelWidth - 2 * buttonWidth - sideMargin) objHeight middle
        buttons = flow right
            [ restartButton
            , fittedHotSwapButton
            , (if state.paused then playButton else pauseButton)
            ]
        slider = flow down
            [ sliderEventText midWidth state
            , scrubSlider (midWidth, h) state
            , sliderInfoText midWidth state
            ]
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
        watchView = flow right
            [ spacer 20 1
            , watches |> textStyle |> leftAligned |> width w
            ]
    in  flow down
            [ controlsContainer
            , bar
            , watchView
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

port watches : Signal String

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

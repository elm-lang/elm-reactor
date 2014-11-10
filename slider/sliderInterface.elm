module SliderInterface where

import Window
import Graphics.Input (..)
import Graphics.Element as GE
import Text (..)
import Slider (..)

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

buttonHeight = 40
buttonWidth = 40
sideMargin = 2 * 20
textHeight = 20
panelWidth = 275

blue = rgb 28 129 218
lightGrey = rgb 228 228 228
darkGrey = rgb 74 74 74

dataStyle : [String] -> Float -> String -> Text
dataStyle typefaces height string =
    let myStyle =
             { defaultStyle
             | typeface <- typefaces
             , color <- lightGrey
             , height <- Just height
             }
    in
        style myStyle (toText string)

textStyle : String -> Text
textStyle = dataStyle ["Gotham", "Futura", "Lucida Grande", "sans-serif"] 12

--
-- View
--

myButton : Handle a -> a -> String -> Element
myButton handle value name =
    let img state = image 40 40 ("/_reactor/debugger/" ++ name ++ "-button-" ++ state ++ ".png")
    in  customButton handle value (img "up") (img "hover") (img "down")

playButton : Element
playButton =
    myButton pausedInput.handle False "play"

pauseButton : Element
pauseButton =
    myButton pausedInput.handle True "pause"

restartButton : Element
restartButton =
    myButton restartInput.handle () "restart"

swapButton : Bool -> Element
swapButton permitSwap =
    let hsWidth = 25
        radius = 4
        bgButton = roundedSquare hsWidth radius (filled lightGrey)
        trueButton = [bgButton, roundedSquare 22 radius (filled blue)]
        trueButtonHover =
            [ bgButton
            , roundedSquare 22 radius (filled blue)
            , roundedSquare 22 radius (filled darkGrey) |> alpha 0.1
            ]
        trueButtonClick = falseButton
        falseButton = [bgButton, roundedSquare 22 radius (filled darkGrey)]
        falseButtonHover =
            [ bgButton
            , roundedSquare 22 radius (filled darkGrey)
            , roundedSquare 22 radius (filled blue) |> alpha 0.1
            ]
        falseButtonClick = trueButton
        button =
            if  | permitSwap ->
                    customButton permitSwapInput.handle False
                        (collage hsWidth hsWidth trueButton)
                        (collage hsWidth hsWidth trueButtonHover)
                        (collage hsWidth hsWidth trueButtonClick)
                | otherwise ->
                    customButton permitSwapInput.handle True
                        (collage hsWidth hsWidth falseButton)
                        (collage hsWidth hsWidth falseButtonHover)
                        (collage hsWidth hsWidth falseButtonClick)
        info = "swap" |> textStyle |> leftAligned
    in  flow right [ info, spacer 10 1, button ]

scrubSlider : (Int, Int) -> State -> Element
scrubSlider (w,_) state =
    let sliderLength = w
        sliderStyle =
            { defaultSlider
            | length <- sliderLength
            , max <- toFloat state.totalEvents
            , value <- toFloat state.scrubPosition
            }
    in  slider scrubInput.handle round sliderStyle
            |> container sliderLength 20 middle

sliderEventText : Int -> State -> Element
sliderEventText w state =
    let textWidthOffset = 14
        scrubPosition = toFloat state.scrubPosition
        totalEvents = toFloat state.totalEvents
        midWidth = toFloat w - sideMargin - textWidthOffset
        leftDistance = 
            if  | totalEvents == 0 -> sideMargin/2 + textWidthOffset/2
                | otherwise ->
                    scrubPosition / totalEvents * midWidth + sideMargin/2 + textWidthOffset/2
        xPos = absolute (round leftDistance)
        yPos = absolute (round (textHeight / 2))
        textPosition = middleAt xPos yPos
        text' = show state.scrubPosition |> textStyle |> centered
    in
        container w textHeight textPosition text'

sliderMinMaxText : Int -> State -> Element
sliderMinMaxText w state =
    let sliderStartText =
            textStyle "0"
                |> leftAligned
                |> container w textHeight topLeft
        sliderTotalEvents =
            show state.totalEvents
                |> textStyle
                |> rightAligned
                |> container w textHeight topRight
    in  flow outward
            [ sliderStartText
            , sliderTotalEvents
            ]

controlView : (Int, Int) -> Bool -> State -> Element
controlView (w, h) permitSwap state =
    let midWidth = w - sideMargin
        topSpacerHeight = 15
        buttonSliderSpaceHeight = 10
        fittedSwapButton =
            if  | showSwap ->
                    swapButton permitSwap
                        |> container (w - 2 * buttonWidth - sideMargin) buttonHeight middle
                | otherwise -> spacer (2 * buttonWidth) 1
        buttons = flow right
            [ restartButton
            , fittedSwapButton
            , (if state.paused then playButton else pauseButton)
            ]
        centeredSliderContainer = flow down
            [ scrubSlider (midWidth, h) state
            , sliderMinMaxText midWidth state
            ]
            |> container w (24 + textHeight) midTop
        slider = flow down
            [ sliderEventText w state
            , centeredSliderContainer
            ]
            |> container w (24 + 2* textHeight) midTop
        buttonContainer = container w buttonHeight midTop buttons
        controls = flow down
            [ spacer midWidth topSpacerHeight
            , buttonContainer
            , spacer midWidth buttonSliderSpaceHeight
            , slider
            , spacer midWidth 10
            ]
        bar = flow down 
            [ spacer w 1 |> GE.color lightGrey |> opacity 0.3
            , spacer w 12
            ]
    in flow down [ controls, bar ]

--
-- The wiring
--

main : Signal Element
main = controlView <~ ((\(w, h) -> (panelWidth, h)) <~ Window.dimensions)
                    ~ permitSwapInput.signal
                    ~ scene

port scrubTo : Signal Int
port scrubTo = .scrubPosition <~ scene

port pause : Signal Bool
port pause = .paused <~ scene

port restart : Signal Int
port restart = lift (\x -> 0) restartInput.signal

port permitSwap : Signal Bool
port permitSwap = permitSwapInput.signal

pausedInput : Input Bool
pausedInput = input False

permitSwapInput : Input Bool
permitSwapInput = input True

restartInput : Input ()
restartInput = input ()

scrubInput : Input Int
scrubInput = input 0

port eventCounter : Signal Int

port showSwap : Bool

scene : Signal State
scene = foldp step startState aggregateUpdates

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
    in
        group [xRect, yRect, tl, tr, bl, br]



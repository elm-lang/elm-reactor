module DebuggerInterface where

import Window
import Graphics.Input (..)
import Graphics.Element as GE
import Text (..)
import Slider (..)
import Debug

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
sideMargin = 2 * 20
textHeight = 20
panelWidth = 275

blue = rgb 28 129 218
lightGrey = rgb 228 228 228
darkGrey = rgb 74 74 74

dataStyle : [String] -> Float -> String -> Text
dataStyle typefaces height =
    let myStyle =
             { defaultStyle
             | typeface <- typefaces
             , color <- lightGrey
             , height <- Just height
             }
    in style myStyle . toText

textStyle : String -> Text
textStyle = dataStyle ["Gotham", "sans-serif"] 12

watchStyle : String -> Text
watchStyle = dataStyle ["Gotham", "sans-serif"] 14

codeStyle : String -> Text
codeStyle = dataStyle ["Menlo for Powerline", "monospace"] 12

--
-- View
--

playButton : Element
playButton =
    customButton pausedInput.handle False
        (image 40 40 "/assets/scaled/play-button.png")
        (image 40 40 "/assets/scaled/play-button-hover.png")
        (image 40 40 "/assets/scaled/play-button-click.png")

pauseButton : Element
pauseButton =
    customButton pausedInput.handle True
        (image 40 40 "/assets/scaled/pause-button.png")
        (image 40 40 "/assets/scaled/pause-button-hover.png")
        (image 40 40 "/assets/scaled/pause-button-click.png")

restartButton : Element
restartButton =
    customButton restartInput.handle ()
        (image 40 40 "/assets/scaled/restart-button.png")
        (image 40 40 "/assets/scaled/restart-button-hover.png")
        (image 40 40 "/assets/scaled/restart-button-click.png")

hotswapButton : Bool -> Element
hotswapButton permitHotswap =
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
            if  | permitHotswap ->
                    customButton permitHotswapInput.handle False
                        (collage hsWidth hsWidth trueButton)
                        (collage hsWidth hsWidth trueButtonHover)
                        (collage hsWidth hsWidth trueButtonClick)
                | otherwise ->
                    customButton permitHotswapInput.handle True
                        (collage hsWidth hsWidth falseButton)
                        (collage hsWidth hsWidth falseButtonHover)
                        (collage hsWidth hsWidth falseButtonClick)
        info = "hotswap" |> textStyle |> leftAligned
    in  flow right [ info, spacer 10 1, button ]

scrubSlider : (Int, Int) -> State -> Element
scrubSlider (w,_) state =
    let sliderLength = w
        sliderStyle =
            { defaultSlider
            | length <- sliderLength
            , max <- toFloat <| state.totalEvents
            , value <- toFloat <| state.scrubPosition
            }
    in  slider scrubInput.handle round sliderStyle
            |> container sliderLength 20 middle

sliderEventText : Int -> State -> Element
sliderEventText w state =
    let textHeight = 20
        textWidthOffset = 14
        scrubPosition = toFloat state.scrubPosition
        totalEvents = toFloat state.totalEvents
        midWidth = (toFloat w) - sideMargin - textWidthOffset
        leftDistance = 
            if  | totalEvents == 0 -> sideMargin/2 + textWidthOffset/2
                | otherwise ->
                    scrubPosition / totalEvents * midWidth + (sideMargin/2) + textWidthOffset/2
        xPos = absolute (round leftDistance)
        yPos = absolute (round (textHeight / 2))
        textPosition = middleAt xPos yPos
        text' = show state.scrubPosition |> textStyle |> centered
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

view : (Int, Int) -> [(String, String)] -> Bool -> State -> Element
view (w,h) watches permitHotswap state =
    let midWidth = w - sideMargin
        topSpacerHeight = 15
        buttonSliderSpaceHeight = 10
        controlsHeight = objHeight + 24 + topSpacerHeight + 2 * textHeight + buttonSliderSpaceHeight
        fittedHotSwapButton =
            hotswapButton permitHotswap
            |> container (w - 2 * buttonWidth - sideMargin) objHeight middle
        buttons = flow right
            [ restartButton
            , fittedHotSwapButton
            , (if state.paused then playButton else pauseButton)
            ]
        centeredSliderContainer = flow down
            [ scrubSlider (midWidth, h) state
            , sliderInfoText midWidth state
            ]
            |> container w (24 + textHeight) midTop
        slider = flow down
            [ sliderEventText w state
            , centeredSliderContainer
            ]
            |> container w (24 + 2* textHeight) midTop
        buttonContainer = container w objHeight midTop buttons
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
        showWatch (k,v) = flow down
            [ k |> watchStyle |> bold |> leftAligned |> width w
            , v |> codeStyle |> leftAligned |> width w
            , spacer 1 12
            ]
        watchView = flow right
            [ spacer 20 1
            , case watches of
                [] -> noWatches
                ws -> map showWatch ws |> flow down
            ]
    in  flow down
            [ controls
            , bar
            , watchView
            ]
        

--
-- The wiring
--

main : Signal Element
main = view <~ ((\(w, h) -> (panelWidth, h)) <~ Window.dimensions)
             ~ watches
             ~ permitHotswapInput.signal
             ~ scene

port scrubTo : Signal Int
port scrubTo = .scrubPosition <~ scene

port pause : Signal Bool
port pause = .paused <~ scene

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

port watches : Signal [(String, String)]

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
        { state | scrubPosition <- pos
                , paused <- True}

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


--
-- Copy
--

noWatches : Element
noWatches = [markdown|

### <span style="font-family: Gotham; font-size: 12pt; color: rgb(170,170,170)"> You don't have any watches! </span>

<span style="color: rgb(170,170,170)">
<span style="font-family: Gotham; font-size: 10pt; color: rgb(170,170,170)">
Use [<span style="text-decoration:underline; color: rgb(170,170,170)">Debug.watch</span>](http://library.elm-lang.org/catalog/elm-lang-Elm/0.12.3/Debug#watch)
to show any value. <br>
`watch : String -> a -> a`</span>

<span style="font-family: Gotham; font-size: 10pt; color: rgb(170,170,170)">
Use [<span style="text-decoration:underline; color: rgb(170,170,170)">Debug.watchSummary</span>](http://library.elm-lang.org/catalog/elm-lang-Elm/0.12.3/Debug#watchSummary) to show a <br>
summary or subvalue of any value. </span><br>
|]



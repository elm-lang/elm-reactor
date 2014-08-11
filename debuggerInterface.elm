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

blue = rgb 49 139 255
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
            |> container sliderLength 20 middle

--sliderEventText : Int -> State -> Element
--sliderEventText w state =
--    let textHeight = 20
--        scrubPosition = toFloat state.scrubPosition
--        totalEvents = toFloat state.totalEvents
--        midWidth = (toFloat w) - sideMargin
--        leftDistance = 
--            if  | totalEvents == 0 -> sideMargin / 2
--                | otherwise ->
--                    scrubPosition / totalEvents * midWidth + (sideMargin/2)
--        _ = Debug.log "leftDistance" (round leftDistance)
--        xPos = absolute (round leftDistance)
--        yPos = absolute (round (textHeight / 2))
--        textPosition = midLeftAt xPos yPos
--        text' = show state.scrubPosition |> textStyle |> centered
--    in  container (round leftDistance) textHeight midRight text'


sliderEventText : Int -> State -> Element
sliderEventText w state =
    let textHeight = 20
        scrubPosition = toFloat state.scrubPosition
        totalEvents = toFloat state.totalEvents
        innerWidth = toFloat <| w - sideMargin
        w' = toFloat w
        leftDistance = scrubPosition / totalEvents * innerWidth + 20
        text' = show state.scrubPosition |> textStyle |> rightAligned
    in  container (round leftDistance) textHeight midRight text'

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
        spacerHeight = 15
        controlsHeight = objHeight + 24 + spacerHeight + 2 * textHeight
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
            [ spacer midWidth spacerHeight
            , buttonContainer
            , slider
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
            , map showWatch watches |> flow down
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

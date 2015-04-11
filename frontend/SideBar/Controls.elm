module SideBar.Controls where

import Color
import Graphics.Collage exposing (..)
import Graphics.Element as GE exposing (..)
import Graphics.Input exposing (..)
import List
import Signal as S exposing (Signal, (<~), (~))
import Slider exposing (..)
import Text

import SideBar.Model as Model


-- STYLE

buttonHeight = 40
buttonWidth = 40
sideMargin = 2 * 20
textHeight = 20
panelWidth = 275

blue = Color.rgb 28 129 218
lightGrey = Color.rgb 228 228 228
darkGrey = Color.rgb 74 74 74


dataStyle : List String -> Float -> String -> Text.Text
dataStyle typefaces height string =
    let default = Text.defaultStyle
        myStyle =
             { default |
                typeface <- typefaces,
                color <- lightGrey,
                height <- Just height
             }
    in
        Text.style myStyle (Text.fromString string)


textTypefaces : List String
textTypefaces =
    ["Gotham", "Futura", "Lucida Grande", "sans-serif"]


textStyle : String -> Text.Text
textStyle string =
    dataStyle textTypefaces 12 string


-- VIEW

myButton : Signal.Message -> String -> Element
myButton message name =
    let img state =
          image 40 40 ("/_reactor/debugger/" ++ name ++ "-button-" ++ state ++ ".png")
    in
        customButton message (img "up") (img "hover") (img "down")


playButton : Element
playButton =
    myButton (Signal.message pausedInputMailbox.address False) "play"


pauseButton : Element
pauseButton =
    myButton (Signal.message pausedInputMailbox.address True) "pause"


pausedInputMailbox : Signal.Mailbox Bool
pausedInputMailbox =
    Signal.mailbox False


restartButton : Element
restartButton =
    myButton (Signal.message restartMailbox.address ()) "restart"


restartMailbox : Signal.Mailbox ()
restartMailbox =
    Signal.mailbox ()


swapButton : Bool -> Element
swapButton permitSwap =
    let hsWidth = 25
        radius = 4
        bgButton =
            roundedSquare hsWidth radius (filled lightGrey)

        trueButton =
            [bgButton, roundedSquare 22 radius (filled blue)]

        trueButtonHover =
            [ bgButton
            , roundedSquare 22 radius (filled blue)
            , roundedSquare 22 radius (filled darkGrey) |> alpha 0.1
            ]

        trueButtonClick = falseButton

        falseButton =
            [bgButton, roundedSquare 22 radius (filled darkGrey)]

        falseButtonHover =
            [ bgButton
            , roundedSquare 22 radius (filled darkGrey)
            , roundedSquare 22 radius (filled blue) |> alpha 0.1
            ]

        falseButtonClick = trueButton

        button =
            case permitSwap of
              True ->
                customButton (Signal.message permitSwapMailbox.address False)
                    (collage hsWidth hsWidth trueButton)
                    (collage hsWidth hsWidth trueButtonHover)
                    (collage hsWidth hsWidth trueButtonClick)
              False ->
                customButton (Signal.message permitSwapMailbox.address True)
                    (collage hsWidth hsWidth falseButton)
                    (collage hsWidth hsWidth falseButtonHover)
                    (collage hsWidth hsWidth falseButtonClick)

        info = GE.leftAligned (textStyle "swap")
    in
        flow right [ info, spacer 10 1, button ]


permitSwapMailbox : Signal.Mailbox Bool
permitSwapMailbox =
    Signal.mailbox True


scrubSlider : (Int, Int) -> Model.Model -> Element
scrubSlider (w,_) state =
    let sliderLength = w

        sliderStyle =
            { defaultSlider |
                length <- sliderLength,
                max <- toFloat state.totalEvents,
                value <- toFloat state.scrubPosition
            }
    in
        slider (\n -> Signal.message scrubMailbox.address (round n)) sliderStyle
            |> container sliderLength 20 middle


scrubMailbox : Signal.Mailbox Int
scrubMailbox =
    Signal.mailbox 0


sliderEventText : Int -> Model.Model -> Element
sliderEventText w state =
    let textWidthOffset = 14

        scrubPosition =
            toFloat state.scrubPosition

        totalEvents =
            toFloat state.totalEvents

        midWidth =
            toFloat w - sideMargin - textWidthOffset

        leftDistance = 
            case totalEvents of
              0 -> sideMargin/2 + textWidthOffset/2
              _ -> scrubPosition / totalEvents * midWidth + sideMargin/2 + textWidthOffset/2

        xPos = absolute (round leftDistance)
        yPos = absolute (round (textHeight / 2))

        textPosition = middleAt xPos yPos

        text' =
          GE.centered (textStyle (toString state.scrubPosition))
    in
        container w textHeight textPosition text'


sliderMinMaxText : Int -> Model.Model -> Element
sliderMinMaxText w state =
    let sliderStartText =
            textStyle "0"
                |> GE.leftAligned
                |> container w textHeight topLeft

        sliderTotalEvents =
            toString state.totalEvents
                |> textStyle
                |> GE.rightAligned
                |> container w textHeight topRight
    in
        flow outward
            [ sliderStartText
            , sliderTotalEvents
            ]


view : (Int, Int) -> Bool -> Bool -> Model.Model -> Element
view (w,h) showSwap permitSwap state =
    let midWidth = w - sideMargin

        topSpacerHeight = 15

        buttonSliderSpaceHeight = 10

        fittedSwapButton =
            if showSwap then
                swapButton permitSwap
                  |> container (w - 2 * buttonWidth - sideMargin) buttonHeight middle
            else
                spacer (2 * buttonWidth) 1

        buttons =
            flow right
            [ restartButton
            , fittedSwapButton
            , if state.paused then playButton else pauseButton
            ]

        centeredSliderContainer =
          container w (24 + textHeight) midTop <|
            flow down
            [ scrubSlider (midWidth, h) state
            , sliderMinMaxText midWidth state
            ]

        slider =
          container w (24 + 2* textHeight) midTop <|
            flow down
            [ sliderEventText w state
            , centeredSliderContainer
            ]

        buttonContainer =
            container w buttonHeight midTop buttons

        controls =
            flow down
            [ spacer midWidth topSpacerHeight
            , buttonContainer
            , spacer midWidth buttonSliderSpaceHeight
            , slider
            , spacer midWidth 10
            ]

        bar =
            opacity 0.3 (color lightGrey (spacer w 1))
    in
        flow down
          [ controls
          , bar
          ]


-- UTILITIES

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


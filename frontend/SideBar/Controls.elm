module SideBar.Controls where

import Color
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import String
import FontAwesome

import Button
import SideBar.Model as Model
import Styles exposing (..)

-- STYLE

buttonIconSize = 20
buttonSideLength = 40
buttonBorderRadius = 8
margin = 20
eventIdxTextHeight = 15

sliderHeight = 20
sliderPadding = 10

{- Would be nice to have an abstraction for elements that know their height...
Like Graphics.Element! Unfortunately it doesn't do everything we need
and we don't want to mix them. -}
totalHeight : Int
totalHeight =
  buttonSideLength + sliderPadding + sliderHeight
    + 2 * eventIdxTextHeight + 2 * margin + 4

hoverBrightness : Color.Color -> Button.Model -> Color.Color
hoverBrightness baseColor state =
  case state of
    Button.Up ->
      baseColor

    Button.Down ->
      darker 0.2 baseColor

    Button.Hover ->
      brighter 0.2 baseColor


playPauseButtonColor : Button.Model -> Color.Color
playPauseButtonColor =
  hoverBrightness (Color.rgb 20 131 213)


restartButtonColor : Button.Model -> Color.Color
restartButtonColor =
  hoverBrightness lightGrey


eventNumberTextStyle =
  [ "color" => colorToCss lightGrey
  , "font-family" => textTypefaces
  , "font-size" => "12px"
  ]


swapButtonTextStyle =
  [ "color" => colorToCss lightGrey
  , "font-family" => textTypefaces
  , "font-size" => "14px"
  ]


blue = Color.rgb 28 129 218
lightGrey = Color.rgb 228 228 228
darkGrey = Color.rgb 74 74 74


-- VIEW

playPauseButton : Bool -> Button.Model -> Html
playPauseButton isPlay state =
  let
    icon =
      if isPlay then
        FontAwesome.play Color.white buttonIconSize
      else
        FontAwesome.pause Color.white buttonIconSize

    render state =
      iconButton (playPauseButtonColor state) icon
  in 
    Button.view
        (Signal.forwardTo buttonStateMailbox.address Model.PlayPauseButtonAction)
        pausedInputMailbox.address
        (not isPlay)
        state
        render


pausedInputMailbox : Signal.Mailbox Bool
pausedInputMailbox =
  Signal.mailbox False


restartButton : Button.Model -> Html
restartButton state =
  let
    render st =
      iconButton
        (restartButtonColor st)
        (FontAwesome.undo darkGrey buttonIconSize)
  in 
    Button.view
      (Signal.forwardTo buttonStateMailbox.address Model.RestartButtonAction)
      restartMailbox.address
      ()
      state
      render


restartMailbox : Signal.Mailbox ()
restartMailbox =
  Signal.mailbox ()


buttonStateMailbox : Signal.Mailbox Model.Action
buttonStateMailbox =
  Signal.mailbox Model.NoOp


swapButton : Bool -> Html
swapButton permitSwap =
  input
    [ type' "checkbox"
    , on "change"
        targetChecked
        (Signal.message permitSwapMailbox.address)
    , checked permitSwap
    ]
    []


permitSwapMailbox : Signal.Mailbox Bool
permitSwapMailbox =
  Signal.mailbox True


scrubSlider : Int -> Model.Model -> Html
scrubSlider width state =
  input
    [ type' "range"
    , style
        [ "width" => intToPx width
        , "height" => intToPx sliderHeight
        , "margin" => "0"
        ]
    , Attr.min (toString 0)
    , Attr.max (toString state.totalEvents)
    , Attr.value (toString state.scrubPosition)
    , on "input"
        (at ["target","value"] (customDecoder string String.toInt))
        (Signal.message scrubMailbox.address)
    ]
    []


scrubMailbox : Signal.Mailbox Int
scrubMailbox =
  Signal.mailbox 0


sliderEventText : Int -> Model.Model -> Html
sliderEventText width state =
  div
    [ style
        [ "height" => intToPx eventIdxTextHeight
        , "position" => "relative"
        ]
    ]
    [ positionedText width state.scrubPosition state.totalEvents False ]


sliderMinMaxText : Int -> Model.Model -> Html
sliderMinMaxText width state =
  div
    [ style
        [ "height" => intToPx eventIdxTextHeight
        , "position" => "relative"
        ]
    ]
    [ positionedText width 0 state.totalEvents False
    , positionedText width state.totalEvents state.totalEvents True
    ]


positionedText : Int -> Int -> Int -> Bool -> Html
positionedText width frameIdx totalEvents alwaysRight =
  let
    charWidth =
      10

    numDigits =
      if frameIdx == 0 then
        1
      else
        ceiling <| logBase 10 (toFloat frameIdx)

    textWidth =
      charWidth * numDigits

    sliderOffset =
      8

    xFraction =
      if alwaysRight then
        1
      else
        toFloat frameIdx / toFloat totalEvents

    textCenterpointRange =
      toFloat width - 2 * sliderOffset + 5

    textCenterpoint =
      xFraction * textCenterpointRange

    xPos =
      textCenterpoint - (toFloat textWidth)/2 + sliderOffset
  in
    div
      [ style <|
          eventNumberTextStyle ++
          [ "position" => "absolute"
          , "display" => "inline-block"
          , "left" => intToPx (round xPos)
          ]
      ]
      [ text (toString frameIdx) ]


view : Bool -> Model.Model -> Html
view showSwap state =
  let
    midWidth =
      sidebarWidth - margin * 2

    swapWithLabel =
      div
        [ style swapButtonTextStyle ]
        (if showSwap then [ text "swap", swapButton state.permitSwap ] else [])

    containerStyle =
      node
        "style"
        [ type' "text/css" ]
        [ text buttonContainerCss ]

    buttonContainer =
      div
        [ id "elm-reactor-button-container" ]
        [ restartButton state.restartButtonState
        , swapWithLabel
        , playPauseButton state.paused state.playPauseButtonState
        ]

    sliderContainer =
      div
        [ style
            [ "padding-top" => intToPx sliderPadding ]
        ]
        [ sliderEventText midWidth state
        , scrubSlider midWidth state
        , sliderMinMaxText midWidth state
        ]
  in
    div
      [ style
          [ "padding" => intToPx margin ]
      ]
      [ containerStyle
      , buttonContainer
      , sliderContainer
      ]


-- UTILITIES

iconButton : Color.Color -> Html -> Html
iconButton bgColor iconHtml =
  let
    transPx =
      (buttonSideLength - buttonIconSize) // 2
  in
    div
      [ style
          [ "background-color" => colorToCss bgColor
          , "border-radius" => intToPx buttonBorderRadius
          , "width" => intToPx buttonSideLength
          , "height" => intToPx buttonSideLength
          ]
      ]
      [ div
          [ style
              [ "padding" => intToPx transPx
              , "display" => "inline-block"
              ]
          ]
          [ iconHtml ]
      ]


translateToCss : Int -> Int -> String
translateToCss x y =
  "translate(" ++ intToPx x ++ "," ++ intToPx y ++ ")"


{- This CSS does not work if added to this element as
inline attributes -- the `display:` attributes do not
override each other properly.
See https://css-tricks.com/using-flexbox/ -}
buttonContainerCss : String
buttonContainerCss = """
#elm-reactor-button-container {
  display: -webkit-box;
  display: -moz-box;
  display: -ms-flexbox;
  display: -webkit-flex;
  display: flex;

  -webkit-flex-direction: row;
  flex-direction: row;
  -webkit-justify-content: space-between;
  justify-content: space-between;
  -webkit-align-items: center;
  align-items: center;
}
"""
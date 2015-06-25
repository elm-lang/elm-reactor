module SideBar.Controls where

import Color
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import List
import String
import Signal as S exposing (Signal, (<~), (~))
import FontAwesome

import Button
import SideBar.Model as Model
import Styles exposing (..)


-- STYLE

buttonIconSize = 20
buttonSideLength = 40
buttonBorderRadius = 8
sideMargin = 20
textHeight = 20
panelWidth = 275


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


blue = Color.rgb 28 129 218
lightGrey = Color.rgb 228 228 228
darkGrey = Color.rgb 74 74 74


(=>) = (,)


eventNumberTextStyle =
  [ "color" => colorToCss lightGrey
  , "font-face" => textTypefaces
  ]


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
  -- TODO: put text "swap" next to it
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
    , style ["width" => intToPx width]
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
        [ "height" => intToPx textHeight
        , "position" => "relative"
        ]
    ]
    [ positionedText width state.scrubPosition state.totalEvents False ]


sliderMinMaxText : Int -> Model.Model -> Html
sliderMinMaxText width state =
  div
    [ style
        [ "height" => intToPx textHeight
        , "position" => "relative"
        ]
    ]
    [ positionedText width 0 state.totalEvents False
    , positionedText width state.totalEvents state.totalEvents True
    ]


positionedText : Int -> Int -> Int -> Bool -> Html
positionedText width frameIdx totalEvents alwaysRight =
  let
    textWidthOffset =
      14

    xFraction =
      if alwaysRight then
        1
      else
        toFloat frameIdx / toFloat totalEvents

    xPos =
      xFraction * (toFloat width - textWidthOffset)
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


view : Bool -> Bool -> Model.Model -> Html
view showSwap permitSwap state =
  let
    midWidth =
      panelWidth - sideMargin * 2

    topSpacerHeight =
      15

    buttonSliderSpaceHeight =
      10

    fittedSwapButton =
      div
        [ style <|
            eventNumberTextStyle ++
            [ "display" => "inline-block"
            , "position" => "absolute"
            , "transform" => "translate(100%, 50%)"
            ]
        ]
        (if showSwap then [ text "swap", swapButton permitSwap ] else [])

    floatButton floatDir button =
      div
        [ style
            [ "display" => "inline-block"
            , "float" => floatDir
            ]
        ]
        [ button ]

    -- TODO: get these horizontally aligned
    buttonContainer =
      div
        [ style ["height" => "50px"] ]
        [ floatButton "left"
            (restartButton state.restartButtonState)
        , fittedSwapButton
        , floatButton "right"
            (playPauseButton state.paused state.playPauseButtonState)
        ]

    sliderContainer =
      div
        []
        [ sliderEventText midWidth state
        , scrubSlider midWidth state
        , sliderMinMaxText midWidth state
        ]

    controls =
      div
        [ style ["margin" => intToPx sideMargin] ]
        [ buttonContainer
        , sliderContainer
        ]

    bar =
      div
        [ style
            [ "height" => "1px"
            , "width" => intToPx panelWidth
            , "opacity" => "0.3"
            , "background-color" => colorToCss Color.lightGrey
            ]
        ]
        []
  in
    div
      []
      [ controls
      , bar
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
          [ style ["transform" => translateToCss transPx transPx] ]
          [ iconHtml ]
      ]


translateToCss : Int -> Int -> String
translateToCss x y =
  "translate(" ++ intToPx x ++ "," ++ intToPx y ++ ")"


intToPx : Int -> String
intToPx x =
  toString x ++ "px"

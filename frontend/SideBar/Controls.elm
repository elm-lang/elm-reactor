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
sideMargin = 20
textHeight = 20


{- 111 read off of chrome inspector because I can't figure
out how to make the watches scroll properly without knowing
the height of everything.
-}
controlsHeight = 111 + 2 * sideMargin


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
      sidebarWidth - sideMargin * 2

    fittedSwapButton =
      div
        [ style <|
            swapButtonTextStyle ++
            [ "display" => "inline-block"
            , "position" => "absolute"
            , "left" => "110px"
            , "top" => "30px"
            ]
        ]
        (if showSwap then [ text "swap", swapButton state.permitSwap ] else [])

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
        [ style
            [ "height" => "50px"
            , "width" => intToPx midWidth
            ]
        ]
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
        [ style
            [ "margin" => intToPx sideMargin]
        ]
        [ buttonContainer
        , sliderContainer
        ]

    bar =
        div
          [ style
              [ "height" => "1px"
              , "width" => intToPx sidebarWidth
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

module SideBar.Controls where

import Color
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import String
import FontAwesome

import Model
import Debugger.Model as DM
import Debugger.Service as Service
import Styles exposing (..)
import Button

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

playPauseButton : Signal.Address Model.Action -> Bool -> Button.Model -> DM.ActiveAttrs -> Html
playPauseButton addr isPlay state activeAttrs =
  let
    icon =
      if isPlay then
        FontAwesome.play Color.white buttonIconSize
      else
        FontAwesome.pause Color.white buttonIconSize

    render state =
      iconButton (playPauseButtonColor state) icon

    -- TODO: we shouldn't have to know this here
    curFrame =
      DM.curFrameIdx activeAttrs
  in 
    Button.view
      (Signal.forwardTo addr Model.PlayPauseButtonAction)
      commandsAddr
      (DM.ForkFrom curFrame isPlay)
      state
      render


restartButton : Signal.Address Model.Action -> Button.Model -> DM.ActiveAttrs -> Html
restartButton addr state activeAttrs =
  let
    render st =
      iconButton
        (restartButtonColor st)
        (FontAwesome.undo darkGrey buttonIconSize)
  in 
    Button.view
      (Signal.forwardTo addr Model.RestartButtonAction)
      commandsAddr
      -- TODO: this should just be a Reset action
      (DM.ForkFrom 0 <| DM.isPlaying activeAttrs)
      state
      render


swapButton : Signal.Address Model.Action -> Bool -> Html
swapButton addr permitSwap =
  input
    [ type' "checkbox"
    , on "change"
        targetChecked
        (Signal.message addr << Model.PermitSwaps)
    , checked permitSwap
    ]
    []


scrubSlider : Int -> Model.Model -> DM.ActiveAttrs -> Html
scrubSlider width state activeAttrs =
  let
    numFrames =
      DM.numFrames activeAttrs

    curFrame =
      DM.curFrameIdx activeAttrs
  in
    input
      [ type' "range"
      , style
          [ "width" => intToPx width
          , "height" => intToPx sliderHeight
          , "margin" => "0"
          ]
      , Attr.min "0"
      , Attr.max <| toString <| numFrames - 1
      , Attr.value <| toString <| curFrame
      , on "input"
          (at ["target","value"] (customDecoder string String.toInt))
          (\idx ->
            Signal.message
              commandsAddr
              (DM.GetNodeState {start=idx, end=idx} [DM.mainId activeAttrs]))
      ]
      []


sliderEventText : Int -> DM.ActiveAttrs -> Html
sliderEventText width activeAttrs =
  let
    numFrames =
      DM.numFrames activeAttrs

    curFrame =
      DM.curFrameIdx activeAttrs
  in
    div
      [ style
          [ "height" => intToPx eventIdxTextHeight
          , "position" => "relative"
          ]
      ]
      [ positionedText width curFrame numFrames False ]


sliderMinMaxText : Int -> DM.ActiveAttrs -> Html
sliderMinMaxText width activeAttrs =
  let
    numFrames =
      DM.numFrames activeAttrs

    curFrame =
      DM.curFrameIdx activeAttrs
  in
    div
      [ style
          [ "height" => intToPx eventIdxTextHeight
          , "position" => "relative"
          ]
      ]
      [ positionedText width 0 numFrames False
      , positionedText width (numFrames-1) numFrames True
      ]


positionedText : Int -> Int -> Int -> Bool -> Html
positionedText width frameIdx numFrames alwaysRight =
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
        toFloat frameIdx / toFloat (numFrames-1)

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


view : Signal.Address Model.Action -> Model.Model -> DM.ActiveAttrs -> Html
view addr state activeAttrs =
  let
    midWidth =
      sidebarWidth - margin * 2

    swapWithLabel =
      div
        [ style swapButtonTextStyle ]
        [ text "swap"
        , swapButton addr state.permitSwaps
        ]

    buttonContainer =
      div
        [ style
            [ "display" => "-webkit-flex"
            , "-webkit-flex-direction" => "row"
            , "-webkit-justify-content" => "space-between"
            , "-webkit-align-items" => "center"
            ]
        ]
        [ restartButton addr state.restartButtonState activeAttrs
        , swapWithLabel
        , playPauseButton addr (not <| DM.isPlaying activeAttrs) state.playPauseButtonState activeAttrs
        ]

    sliderContainer =
      div
        [ style
            [ "padding-top" => intToPx sliderPadding ]
        ]
        [ sliderEventText midWidth activeAttrs
        , scrubSlider midWidth state activeAttrs
        , sliderMinMaxText midWidth activeAttrs
        ]
  in
    div
      [ style
          [ "padding" => intToPx margin ]
      ]
      [ buttonContainer
      , sliderContainer
      ]

-- TODO: pass this as an argument from main?
-- it never changes...
commandsAddr : Signal.Address DM.Command
commandsAddr =
  (Service.commandsMailbox ()).address




--view : Signal.Address Action -> DM.ActiveAttrs -> Html
--view addr activeAttrs =
--  let
--    ....
--  in
--    div []
--      [ div []
--          [ button
--              [ onClick
--                  commandsAddr
--                  (if DM.isPlaying activeAttrs then
--                    DM.Pause
--                  else
--                    DM.ForkFrom curFrame True)
--              ]
--              [ text
--                  (if DM.isPlaying activeAttrs then "Pause" else "Play")
--              ]
--          , button
--              [ onClick commandsAddr <|
--                  DM.ForkFrom 0 (DM.isPlaying activeAttrs)
--              ]
--              [ text "Reset" ]
--          ]
--      , div []
--          [ div []
--          [ text <|
--              "frame idx: "
--              ++ (toString <| curFrame)
--              ++ "; numFrames: " ++ toString numFrames
--          , input
--              [ type' "range"
--              , Attr.min "0"
--              , Attr.max <| toString <| numFrames - 1
--              , Attr.value <| toString <| curFrame
--              , on
--                  "input"
--                  (JsDec.at ["target","value"]
--                    (JsDec.customDecoder JsDec.string String.toInt))
--                  (\idx ->
--                    Signal.message
--                    commandsAddr
--                    (DM.GetNodeState {start=idx, end=idx} [DM.mainId activeAttrs]))
--              ]
--              []
--            ]
--          ]
--      ]


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

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
import Text

import SideBar.Model as Model
import Styles exposing (..)


-- STYLE

buttonHeight = 40
buttonWidth = 40
sideMargin = 2 * 20
textHeight = 20
panelWidth = 275

blue = Color.rgb 28 129 218
lightGrey = Color.rgb 228 228 228
darkGrey = Color.rgb 74 74 74


--dataStyle : List String -> Float -> String -> Text.Text
--dataStyle typefaces height string =
--    let default = Text.defaultStyle
--        myStyle =
--             { default |
--                typeface <- typefaces,
--                color <- lightGrey,
--                height <- Just height
--             }
--    in
--        Text.style myStyle (Text.fromString string)


--textStyle : String -> Text.Text
--textStyle string =
--    dataStyle textTypefaces 12 string


-- VIEW

-- TODO: replace w/ FontAwesome
myButton : Signal.Address a -> a
         -> Signal.Address Model.ButtonState
         -> Model.ButtonState -> String -> Html
myButton addr action buttonStateAddr buttonState name =
  let
    stateName =
      case buttonState of
        Model.Up -> "up"
        Model.Down -> "down"
        Model.Hover -> "hover"
    path =
      "/_reactor/debugger/" ++ name ++ "-button-" ++ stateName ++ ".png"
  in
    img
      [ src path
      , Attr.width 40
      , Attr.height 40
      , onMouseOver buttonStateAddr Model.Hover
      , onMouseDown buttonStateAddr Model.Down
      , onMouseUp buttonStateAddr Model.Hover
      , onMouseLeave buttonStateAddr Model.Up
      , onClick addr action
      ]
      []


playButton : Model.ButtonState -> Html
playButton state =
    myButton
        pausedInputMailbox.address False
        playPauseButtonStateMailbox.address state "play"


pauseButton : Model.ButtonState -> Html
pauseButton state =
    myButton
        pausedInputMailbox.address True
        playPauseButtonStateMailbox.address state "pause"


pausedInputMailbox : Signal.Mailbox Bool
pausedInputMailbox =
    Signal.mailbox False


playPauseButtonStateMailbox : Signal.Mailbox Model.ButtonState
playPauseButtonStateMailbox =
    Signal.mailbox Model.Up


restartButton : Model.ButtonState -> Html
restartButton state =
    myButton
        restartMailbox.address ()
        restartButtonStateMailbox.address state "restart"


restartMailbox : Signal.Mailbox ()
restartMailbox =
    Signal.mailbox ()


restartButtonStateMailbox : Signal.Mailbox Model.ButtonState
restartButtonStateMailbox =
    Signal.mailbox Model.Up


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


scrubSlider : (Int, Int) -> Model.Model -> Html
scrubSlider (sliderLength,_) state =
  input
    [ type' "range"
    , style [("width", intToPx sliderLength)]
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
sliderEventText w state =
  let
    textWidthOffset = 14

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

    xPos = round leftDistance
    yPos = round (textHeight / 2)

    textPositionStyle =
      [ ("position", "absolute")
      , ("left", intToPx xPos)
      , ("top", intToPx yPos)
      , ("text-align", "center")
      ]
  in
    div
      [ id "slider-event-text-container"
      , style [("width", intToPx w), ("height", intToPx textHeight)]
      ]
      [ div
          [ id "slider-event-text"
          , style textPositionStyle
          ]
          [ text (toString state.scrubPosition) ]
      ]


sliderMinMaxText : Int -> Model.Model -> Html
sliderMinMaxText w state =
    div
      [ id "slider-min-max-text" ]
      [ div
          [ id "slider-min-text"
          , style [("float", "left")]
          ]
          [ text "0" ]
      , div
          [ id "slider-max-text"
          , style [("float", "right")]
          ]
          [ text (toString state.totalEvents) ]
      ]


view : (Int, Int) -> Bool -> Bool -> Model.Model -> Html
view (w,h) showSwap permitSwap state =
    let midWidth = w - sideMargin

        topSpacerHeight = 15

        buttonSliderSpaceHeight = 10

        --fittedSwapButton =
        --    if showSwap then
        --        swapButton permitSwap
        --          |> container (w - 2 * buttonWidth - sideMargin) buttonHeight middle
        --    else
        --        spacer (2 * buttonWidth) 1

        -- TODO: center, make conditional on `showSwap`
        fittedSwapButton =
          swapButton permitSwap

        -- TODO: get these horizontally aligned
        buttonContainer =
          div
            [ id "button-container" ]
            [ restartButton state.restartButtonState
            , fittedSwapButton
            , if state.paused
              then playButton state.playPauseButtonState
              else pauseButton state.playPauseButtonState
            ]

        sliderContainer =
          div
            [ id "slider-container" ]
            [ sliderEventText w state
            , scrubSlider (midWidth, h) state
            , sliderMinMaxText midWidth state
            ]

        controls =
          div
            [ id "controls" ]
            [ buttonContainer
            , sliderContainer
            ]

        bar =
            div
              [ style
                  [ ("height", "1px")
                  , ("width", intToPx w)
                  , ("opacity", "0.3")
                  , ("background-color", colorToCss Color.lightGrey)
                  ]
              ]
              []
    in
        div
          [ id "controls-view" ]
          [ controls
          , bar
          ]


-- UTILITIES

intToPx : Int -> String
intToPx x = toString x ++ "px"

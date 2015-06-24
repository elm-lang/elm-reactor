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
sideMargin = 20
textHeight = 20
panelWidth = 275

blue = Color.rgb 28 129 218
lightGrey = Color.rgb 228 228 228
darkGrey = Color.rgb 74 74 74

eventNumberTextStyle =
    [ ("color", colorToCss lightGrey)
    , ("font-face", textTypefaces)
    ]


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


scrubSlider : Int -> Model.Model -> Html
scrubSlider width state =
  input
    [ type' "range"
    , style [("width", intToPx width)]
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
        [ ("height", intToPx textHeight)
        , ("position", "relative")
        ]
    ]
    [ positionedText width state.scrubPosition state.totalEvents False ]


sliderMinMaxText : Int -> Model.Model -> Html
sliderMinMaxText width state =
    div
      [ style
          [ ("height", intToPx textHeight)
          , ("position", "relative")
          ]
      ]
      [ positionedText width 0 state.totalEvents False
      , positionedText width state.totalEvents state.totalEvents True
      ]

positionedText : Int -> Int -> Int -> Bool -> Html
positionedText width frameIdx totalEvents alwaysRight =
  let
    textWidthOffset = 14

    xFraction =
      if alwaysRight then 1 else (toFloat frameIdx) / (toFloat totalEvents)

    xPos =
      xFraction * (toFloat width - textWidthOffset)
  in
    div
      [ style
          ([ ("position", "absolute")
           , ("display", "inline-block")
           , ("left", intToPx (round xPos))
           ] ++ eventNumberTextStyle)
      ]
      [ text (toString frameIdx) ]

view : Bool -> Bool -> Model.Model -> Html
view showSwap permitSwap state =
    let midWidth = panelWidth - sideMargin * 2

        topSpacerHeight = 15

        buttonSliderSpaceHeight = 10

        fittedSwapButton =
          div
            [ style
                ([ ("display", "inline-block")
                 , ("position", "absolute")
                 , ("transform", "translate(100%, 50%)")
                 ] ++ eventNumberTextStyle)
            ]
            (if showSwap
             then [ text "swap"
                  , swapButton permitSwap
                  ]
             else [])

        floatButton floatDir button =
          div
            [ style
                [ ("display", "inline-block")
                , ("float", floatDir)
                ]
            ]
            [ button ]

        rightButton =
            if state.paused
            then playButton state.playPauseButtonState
            else pauseButton state.playPauseButtonState

        -- TODO: get these horizontally aligned
        buttonContainer =
          div
            [ style [("height", "50px")] ]
            [ floatButton "left" (restartButton state.restartButtonState)
            , fittedSwapButton
            , floatButton "right" rightButton
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
            [ style [("margin", intToPx sideMargin)] ]
            [ buttonContainer
            , sliderContainer
            ]

        bar =
            div
              [ style
                  [ ("height", "1px")
                  , ("width", intToPx panelWidth)
                  , ("opacity", "0.3")
                  , ("background-color", colorToCss Color.lightGrey)
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

intToPx : Int -> String
intToPx x = toString x ++ "px"
